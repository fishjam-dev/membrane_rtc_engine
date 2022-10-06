# Custom endpoint

This section outlines how to implement custom RTC Engine endpoint.

Because publishing is a little bit harder than subscribing and 
there is only one endpoint that publishes track (WebRTC endpoint)
which is also a little bit specific, we will focus on subscribing and
create recording endpoint that will save tracks published by WebRTC endpoint.

It is recommended to read [Track Lifcycle](guides/track_lifecycle.md) section before
diving deeper in this guide.
You will also find there some information about publishing track.

## General rules

Each RTC Engine endpoint has to:
* implement `Membrane.Bin` behavior
* has at least one Membrane element that will be responsible for producing or
consuming data as Membrane bins are used only for logical grouping - they
don't process data
* publish or/and subscribe for tracks

## Implementing `Membrane.Bin`

To implement `Membrane.Bin` behavior we have to: 
* `use Membrane.Bin`
* specify some configuration options (if needed)
* specify input and/or output pads 
* implement some callbacks.

Pads depend on what our bin is intended to do.
For example, if our endpoint does not publish any tracks but only subscribes for tracks from other 
endpoints it can specify only input pads.
Pads should be in the following form

```elixir
def_input_pad :input,
    demand_unit: :buffers,
    caps: <caps>,
    availability: :on_request

def_output_pad :output,
    demand_unit: :buffers,
    caps: <caps>,
    availability: :on_request
```

Where `caps` are `t:Membrane.Caps.t/0` or `:any`.

When it comes to endpoint options we will have:
* `rtc_engine` - pid of RTC engine. It will be used for subscribing for a track
* `output_dir_path` - output directory path i.e. where our tracks should be saved.

Putting it all together we have

```elixir
defmodule RecordingEndpoint do
  @moduledoc """
  Endpoint for saving tracks to a disk.

  Each track will be saved in a separate file.
  """
  use Membrane.Bin

  alias Membrane.RTC.Engine

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              output_dir_path: [
                spec: Path.t(),
                description: "Path to directory, where tracks will be saved."
              ]

  def_input_pad :input,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request

  @impl true
  def handle_init(opts) do
    state = %{
      directory_path: opts.directory_path,
      rtc_engine: opts.rtc_engine
    }

    {:ok, state}
  end
end
```

## Consuming data

Because `Membrane.Bin`s are not capable of sending or consuming any data on their own 
(they can be more thought of as logical containers) we have to add at least one 
casual Membrane element that will do this for us.

In our case, we will need two elements:
* [`Membrane.Stream.Serializer`](https://hexdocs.pm/membrane_stream_plugin/Membrane.Stream.Serializer.html) -
used for serializing incoming Membrane stream
* `Membrane.File.Sink` - used for saving serialized data into a file 

For each incoming track we will create separate serializer and sink.

## Subscribing for a track

Whenever some endpoint publishes its tracks, all other endpoints receive message
in form of `{:new_tracks, tracks}` where `tracks` is a list of `t:Membrane.RTC.Engine.Track.t/0`.

To subscribe for any of published tracks endpoint has to call `Membrane.RTC.Engine.subscribe/4`. 

After subscribing for a track an endpoint will be notified about its readiness in
`c:Membrane.Bin.handle_pad_added/3` callback. An example implementation of `handle_pad_added`
callback can look like this

```elixir
@impl true
def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
  children = %{
    {:track_receiver, track_id} => %TrackReceiver{
      track: Map.fetch!(state.tracks, track_id),
      initial_target_variant: :high
    },
    {:serializer, track_id} => Membrane.Stream.Serializer,
    {:sink, track_id} => %Membrane.File.Sink{
      location: Path.join(state.output_dir_path, track_id)
    }
  }

  links = [
    link_bin_input(pad)
    |> to({:track_receiver, track_id})
    |> to({:serializer, track_id})
    |> to({:sink, track_id})
  ]

  {{:ok, spec: %ParentSpec{children: children, links: links}}, state}
end
```

We simply link our input pad on which we will receive media data to serializer
and then from serializer to sink.

Before serializer we also plugged in one more element called `TrackReceiver`.
It is responsible for requesting the highest possible track variant and
switching to other variants when the currently used one becomes inactive.

> #### Deeper dive into TrackReciver {: .tip}
>
> After linking input pad for given track, our endpoint will start receiving events.
> The very first event is always `TrackVariantResumed`. It informs that given track
> variant is available and can be requested with `RequestTrackVariant` event. 
> Engine ensures that each endpoint always receive track variant starting from
> a keyframe therefore before receiving first media packets our endpoint 
> will receive `TrackVariantSwitched` event and finally media packets will
> start flowing.
> 
> When some track variant was paused we will receive `TrackVariantPaused` event.
>
> The whole process is shown in the following diagram
> 
> ```ascii
> Engine ---- TrackVariantResumed (:medium) ---> Endpoint
> Engine <--- RequestTrackVariant (:medium) ---- Endpoint
> Engine ---- TrackVariantResumed (:low)    ---> Endpoint
> Engine ---- TrackVariantSwitched ---> Endpoint
> Engine ----        media         ---> Endpoint
> Engine ---- TrackVariantPaused  (:medium) ---> Endpoint
> Engine <--- RequestTrackVariant (:low)    ---- Endpoint
> ```
>
> Beacause this logic must be duplicated in each endpoint we encapsulated 
> it into `TrackReceiver` that can easily be plugged in before actual
> Membrane element.
>

Endpoint will be also notified when some tracks it subscribed for are removed with
`{:removed_tracks, tracks}` message.


Putting it all together

```elixir
defmodule RecordingEndpoint do
  @moduledoc """
  Endpoint for saving tracks to a disk.

  Each track will be saved in a separate file.
  """
  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver

  def_options(
    rtc_engine: [
      spec: pid(),
      description: "Pid of parent Engine"
    ],
    output_dir_path: [
      spec: Path.t(),
      description: "Path to directory, where tracks will be saved."
    ]
  )

  def_input_pad(:input,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request
  )

  @impl true
  def handle_init(opts) do
    state = %{
      output_dir_path: opts.output_dir_path,
      rtc_engine: opts.rtc_engine,
      tracks: %{}
    }

    {:ok, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
    children = %{
      {:track_receiver, track_id} => %TrackReceiver{
        track: Map.fetch!(state.tracks, track_id),
        initial_target_variant: :high
      },
      {:serializer, track_id} => Membrane.Stream.Serializer,
      {:sink, track_id} => %Membrane.File.Sink{
        location: Path.join(state.output_dir_path, track_id)
      }
    }

    links = [
      link_bin_input(pad)
      |> to({:track_receiver, track_id})
      |> to({:serializer, track_id})
      |> to({:sink, track_id})
    ]

    {{:ok, spec: %ParentSpec{children: children, links: links}}, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:output, track_id), _ctx, state) do
    children = [
      {:track_receiver, track_id},
      {:serialized, track_id},
      {:sink, track_id}
    ]

    {{:ok, remove_child: children}, state}
  end

  @impl true
  def handle_other({:new_tracks, tracks}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    Enum.reduce_while(tracks, {:ok, state}, fn track, {:ok, state} ->
      case Engine.subscribe(state.rtc_engine, endpoint_id, track.id) do
        :ok ->
          {:cont, {:ok, update_in(state, [:tracks], &Map.put(&1, track.id, track))}}

        {:error, :invalid_track_id} ->
          Membrane.Logger.warn("""
          Couldn't subscribe to track: #{inspect(track.id)}. No such track.
          It had to be removed just after publishing it. Ignoring.
          """)

          {:cont, {:ok, state}}

        {:error, reason} ->
          raise "Couldn't subscribe for track: #{inspect(track.id)}. Reason: #{inspect(reason)}"
      end
    end)
  end

  @impl true
  def handle_other({:remove_tracks, []}, _ctx, state) do
    {:ok, state}
  end
end
```