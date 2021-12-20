if Enum.all?(
     [Membrane.H264.FFmpeg.Parser, Membrane.HTTPAdaptiveStream.SinkBin, Membrane.AAC.FDK.Encoder],
     &Code.ensure_loaded?/1
   ) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS do
    @moduledoc """
    An Endpoint responsible for converting incoming tracks to HLS playlist.
    """
    use Membrane.Bin
    alias Membrane.RTC.Engine
    require Membrane.Logger

    def_input_pad :input,
      demand_unit: :buffers,
      caps: :any,
      availability: :on_request

    def_options output_directory: [
                  spec: Path.t(),
                  description: "Path to directory under which HLS output will be saved",
                  default: "hls_output"
                ]

    @impl true
    def handle_init(opts) do
      state = %{
        tracks: %{},
        stream_ids: MapSet.new(),
        output_directory: opts.output_directory
      }

      {:ok, state}
    end

    @impl true
    def handle_other({:new_tracks, tracks}, _ctx, state) do
      new_tracks = Map.new(tracks, &{&1.id, &1})

      subscriptions =
        Enum.filter(tracks, fn track -> :raw in track.format end)
        |> Enum.map(fn track -> {track.id, :raw} end)

      actions = Engine.subscribe(subscriptions)
      {{:ok, actions}, Map.update!(state, :tracks, &Map.merge(&1, new_tracks))}
    end

    @impl true
    def handle_other(msg, _ctx, state) do
      Membrane.Logger.warn("Unexpected message: #{inspect(msg)}. Ignoring.")
      {:ok, state}
    end

    @impl true
    def handle_notification(notification, _element, _context, state) do
      Membrane.Logger.warn("Unexpected notification: #{inspect(notification)}. Ignoring.")
      {:ok, state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
      link_builder = link_bin_input(pad)
      track = Map.get(state.tracks, track_id)

      directory = Path.join(state.output_directory, track.stream_id)

      # remove directory if it already exists
      File.rm_rf(directory)
      File.mkdir_p!(directory)

      spec = hls_links_and_children(link_builder, track.encoding, track_id, track.stream_id)

      {spec, state} =
        if MapSet.member?(state.stream_ids, track.stream_id) do
          {spec, state}
        else
          hls_sink_bin = %Membrane.HTTPAdaptiveStream.SinkBin{
            manifest_module: Membrane.HTTPAdaptiveStream.HLS,
            target_window_duration: 20 |> Membrane.Time.seconds(),
            target_segment_duration: 2 |> Membrane.Time.seconds(),
            persist?: false,
            storage: %Membrane.HTTPAdaptiveStream.Storages.FileStorage{
              directory: directory
            }
          }

          new_spec = %{
            spec
            | children: Map.put(spec.children, {:hls_sink_bin, track.stream_id}, hls_sink_bin)
          }

          {new_spec, %{state | stream_ids: MapSet.put(state.stream_ids, track.stream_id)}}
        end

      {{:ok, spec: spec}, state}
    end

    defp hls_links_and_children(link_builder, :OPUS, track_id, stream_id),
      do: %ParentSpec{
        children: %{
          {:opus_decoder, track_id} => Membrane.Opus.Decoder,
          {:aac_encoder, track_id} => Membrane.AAC.FDK.Encoder,
          {:aac_parser, track_id} => %Membrane.AAC.Parser{out_encapsulation: :none}
        },
        links: [
          link_builder
          |> to({:opus_decoder, track_id})
          |> to({:aac_encoder, track_id})
          |> to({:aac_parser, track_id})
          |> via_in(Pad.ref(:input, :audio), options: [encoding: :AAC])
          |> to({:hls_sink_bin, stream_id})
        ]
      }

    defp hls_links_and_children(link_builder, :AAC, _track_id, stream_id),
      do: %ParentSpec{
        children: %{},
        links: [
          link_builder
          |> via_in(Pad.ref(:input, :audio), options: [encoding: :AAC])
          |> to({:hls_sink_bin, stream_id})
        ]
      }

    defp hls_links_and_children(link_builder, :H264, track_id, stream_id),
      do: %ParentSpec{
        children: %{
          {:video_parser, track_id} => %Membrane.H264.FFmpeg.Parser{
            framerate: {30, 1},
            alignment: :au,
            attach_nalus?: true
          }
        },
        links: [
          link_builder
          |> to({:video_parser, track_id})
          |> via_in(Pad.ref(:input, :video), options: [encoding: :H264])
          |> to({:hls_sink_bin, stream_id})
        ]
      }
  end
end
