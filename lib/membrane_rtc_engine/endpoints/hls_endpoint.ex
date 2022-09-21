if Enum.all?(
     [
       Membrane.H264.FFmpeg.Parser,
       Membrane.HTTPAdaptiveStream.SinkBin
     ],
     &Code.ensure_loaded?/1
   ) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS do
    @moduledoc """
    An Endpoint responsible for converting incoming tracks to HLS playlist.

    This module requires the following plugins to be present in your `mix.exs` for H264 & AAC input:
    ```
    [
      :membrane_h264_ffmpeg_plugin,
      :membrane_http_adaptive_stream_plugin,
    ]
    ```

    Plus, optionally it supports OPUS audio input - for that, additional dependencies are needed:
    ```
    [
      :membrane_opus_plugin,
      :membrane_aac_plugin,
      :membrane_aac_fdk_plugin,
    ]
    ```
    """
    use Membrane.Bin

    require Membrane.Logger

    alias Membrane.RTC.Engine
    alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
    alias Membrane.RTC.Engine.Track

    @opus_deps [Membrane.Opus.Decoder, Membrane.AAC.Parser, Membrane.AAC.FDK.Encoder]

    def_input_pad :input,
      demand_unit: :buffers,
      caps: :any,
      availability: :on_request

    def_options rtc_engine: [
                  spec: pid(),
                  description: "Pid of parent Engine"
                ],
                output_directory: [
                  spec: Path.t(),
                  description: "Path to directory under which HLS output will be saved",
                  default: "hls_output"
                ],
                owner: [
                  spec: pid(),
                  description: """
                  Pid of parent all notifications will be send to.

                  These notifications are:
                    * `{:playlist_playable, content_type, stream_id, origin}`
                    * `{:cleanup, clean_function, stream_id}`
                  """
                ],
                hls_mode: [
                  spec: :separate_av | :muxed_av,
                  default: :separate_av,
                  description: """
                  Defines output mode for `Membrane.HTTPAdaptiveStream.SinkBin`.

                  - `:separate_av` - audio and video tracks will be separated
                  - `:muxed_av` - audio will be attached to every video track
                  """
                ],
                target_window_duration: [
                  type: :time,
                  spec: Membrane.Time.t() | :infinity,
                  default: Membrane.Time.seconds(20),
                  description: """
                  Max duration of stream that will be stored. Segments that are older than window duration will be removed.
                  """
                ],
                target_segment_duration: [
                  type: :time,
                  spec: Membrane.Time.t(),
                  default: Membrane.Time.seconds(5),
                  description: """
                  Expected length of each segment. Setting it is not necessary, but
                  may help players achieve better UX.
                  """
                ],
                framerate: [
                  spec: {integer(), integer()} | nil,
                  description: """
                  Framerate of tracks
                  """,
                  default: nil
                ]

    @impl true
    def handle_init(opts) do
      state = %{
        rtc_engine: opts.rtc_engine,
        tracks: %{},
        stream_ids: MapSet.new(),
        output_directory: opts.output_directory,
        owner: opts.owner,
        hls_mode: opts.hls_mode,
        target_window_duration: opts.target_window_duration,
        framerate: opts.framerate,
        target_segment_duration: opts.target_segment_duration
      }

      {:ok, state}
    end

    @impl true
    def handle_other({:new_tracks, tracks}, ctx, state) do
      {:endpoint, endpoint_id} = ctx.name
      tracks = Enum.filter(tracks, fn track -> :raw in track.format end)

      state =
        Enum.reduce(tracks, state, fn track, state ->
          case Engine.subscribe(state.rtc_engine, endpoint_id, track.id) do
            :ok ->
              put_in(state, [:tracks, track.id], track)

            {:error, :invalid_track_id} ->
              Membrane.Logger.debug("""
              Couldn't subscribe to track: #{inspect(track.id)}. No such track.
              It had to be removed just after publishing it. Ignoring.
              """)

              state

            {:error, reason} ->
              raise "Couldn't subscribe for track: #{inspect(track.id)}. Reason: #{inspect(reason)}"
          end
        end)

      {:ok, state}
    end

    @impl true
    def handle_other(msg, _ctx, state) do
      Membrane.Logger.warn("Unexpected message: #{inspect(msg)}. Ignoring.")
      {:ok, state}
    end

    def handle_notification(
          {:track_playable, {content_type, track_id}},
          {:hls_sink_bin, stream_id},
          _ctx,
          state
        ) do
      %{origin: origin} = Map.fetch!(state.tracks, track_id)
      # notify about playable just when video becomes available
      send(state.owner, {:playlist_playable, content_type, stream_id, origin})
      {:ok, state}
    end

    def handle_notification(
          {:cleanup, clean_function},
          {:hls_sink_bin, stream_id},
          _ctx,
          state
        ) do
      # notify about possibility to cleanup as the stream is finished.
      send(state.owner, {:cleanup, clean_function, stream_id})
      {:ok, state}
    end

    @impl true
    def handle_notification(notification, _element, _context, state) do
      Membrane.Logger.warn("Unexpected notification: #{inspect(notification)}. Ignoring.")
      {:ok, state}
    end

    @impl true
    def handle_pad_removed(Pad.ref(:input, track_id), ctx, state) do
      children =
        [
          :opus_decoder,
          :aac_encoder,
          :aac_parser,
          :keyframe_requester,
          :video_parser
        ]
        |> Enum.map(&{&1, track_id})
        |> Enum.filter(&Map.has_key?(ctx.children, &1))

      {removed_track, tracks} = Map.pop!(state.tracks, track_id)

      sink_bin_used? =
        Enum.any?(tracks, fn {_id, track} ->
          track.stream_id == removed_track.stream_id
        end)

      children =
        if sink_bin_used?,
          do: children,
          else: [{:hls_sink_bin, removed_track.stream_id} | children]

      stop_timer_action =
        if removed_track.type == :video,
          do: [stop_timer: {:request_keyframe, removed_track.id}],
          else: []

      {{:ok, [remove_child: children] ++ stop_timer_action}, state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
      link_builder = link_bin_input(pad)
      track = Map.get(state.tracks, track_id)

      directory = Path.join(state.output_directory, track.stream_id)

      # remove directory if it already exists
      File.rm_rf(directory)
      File.mkdir_p!(directory)

      spec = hls_links_and_children(link_builder, track.encoding, track, state.framerate)

      {spec, state} =
        if MapSet.member?(state.stream_ids, track.stream_id) do
          {spec, state}
        else
          hls_sink_bin = %Membrane.HTTPAdaptiveStream.SinkBin{
            manifest_module: Membrane.HTTPAdaptiveStream.HLS,
            target_window_duration: state.target_window_duration,
            target_segment_duration: state.target_segment_duration,
            persist?: false,
            storage: %Membrane.HTTPAdaptiveStream.Storages.FileStorage{
              directory: directory
            },
            hls_mode: state.hls_mode
          }

          new_spec = %{
            spec
            | children: Map.put(spec.children, {:hls_sink_bin, track.stream_id}, hls_sink_bin)
          }

          {new_spec, %{state | stream_ids: MapSet.put(state.stream_ids, track.stream_id)}}
        end

      timer_action =
        if track.type == :video do
          [{:start_timer, {{:request_keyframe, track.id}, state.target_segment_duration}}]
        else
          []
        end

      {{:ok, [spec: spec] ++ timer_action}, state}
    end

    @impl true
    def handle_tick({:request_keyframe, track_id}, _ctx, state) do
      actions = [forward: {{:track_receiver, track_id}, :request_keyframe}]
      {{:ok, actions}, state}
    end

    if Enum.all?(@opus_deps, &Code.ensure_loaded?/1) do
      defp hls_links_and_children(link_builder, :OPUS, track, _framerate) do
        %ParentSpec{
          children: %{
            {:track_receiver, track.id} => %TrackReceiver{
              track: track,
              initial_target_variant: :high
            },
            {:depayloader, track.id} => Track.get_depayloader(track),
            {:opus_decoder, track.id} => Membrane.Opus.Decoder,
            {:aac_encoder, track.id} => Membrane.AAC.FDK.Encoder,
            {:aac_parser, track.id} => %Membrane.AAC.Parser{out_encapsulation: :none}
          },
          links: [
            link_builder
            |> to({:track_receiver, track.id})
            |> to({:depayloader, track.id})
            |> to({:opus_decoder, track.id})
            |> to({:aac_encoder, track.id})
            |> to({:aac_parser, track.id})
            |> via_in(Pad.ref(:input, {:audio, track.id}), options: [encoding: :AAC])
            |> to({:hls_sink_bin, track.stream_id})
          ]
        }
      end
    else
      defp hls_links_and_children(_link_builder, :OPUS, _track, _framerate) do
        raise """
        Cannot find one of the modules required to support Opus audio input.
        Ensure `:membrane_opus_plugin`, `:membrane_aac_plugin` and `:membrane_aac_fdk_plugin` are added to the deps.
        """
      end
    end

    defp hls_links_and_children(link_builder, :AAC, track, _framerate),
      do: %ParentSpec{
        children: %{},
        links: [
          link_builder
          |> via_in(Pad.ref(:input, {:audio, track.id}), options: [encoding: :AAC])
          |> to({:hls_sink_bin, track.stream_id})
        ]
      }

    defp hls_links_and_children(link_builder, :H264, track, framerate),
      do: %ParentSpec{
        children: %{
          {:track_receiver, track.id} => %TrackReceiver{
            track: track,
            initial_target_variant: :high
          },
          {:depayloader, track.id} => Track.get_depayloader(track),
          {:video_parser, track.id} => %Membrane.H264.FFmpeg.Parser{
            alignment: :au,
            attach_nalus?: true,
            framerate: framerate
          }
        },
        links: [
          link_builder
          |> to({:track_receiver, track.id})
          |> to({:depayloader, track.id})
          |> to({:video_parser, track.id})
          |> via_in(Pad.ref(:input, {:video, track.id}), options: [encoding: :H264])
          |> to({:hls_sink_bin, track.stream_id})
        ]
      }
  end
end
