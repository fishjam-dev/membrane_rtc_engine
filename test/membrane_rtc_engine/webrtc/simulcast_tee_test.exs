defmodule Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastTeeTest do
  use ExUnit.Case, async: true
  use Bitwise

  import Membrane.Testing.Assertions
  import Membrane.ParentSpec

  alias Membrane.{Buffer, Pad, Time}
  alias Membrane.RTC.Engine.Track
  alias Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastTee
  alias Membrane.Testing.{Pipeline, Sink}

  require Membrane.Pad

  @endpoint_id "endpoint"
  @track_id "track1"
  @layers ["h", "m", "l"]

  defmacro fake_keyframe(layer) do
    quote do
      <<0::1, 0::2, 5::5, unquote(layer)::binary-size(1)>>
    end
  end

  describe "Keyframe Requests are sent" do
    test "is sent when switching layers" do
      track =
        Track.new(
          :video,
          @track_id,
          "generated",
          :H264,
          90_000,
          :RAW,
          nil,
          id: @track_id,
          simulcast_encodings: @layers
        )

      layers_links =
        for layer <- @layers do
          source = %Membrane.RTC.Engine.Testing.Source{
            interval: Time.milliseconds(150),
            payload: fake_keyframe(layer)
          }

          link({:source, layer}, source)
          |> to({:realtimer, layer}, Membrane.Realtimer)
          |> via_in(Pad.ref(:input, {@track_id, layer}))
          |> to(:tee)
        end

      links = [
        link(:tee, %SimulcastTee{track: track})
        |> via_out(Pad.ref(:output, {:endpoint, @endpoint_id}))
        |> to(:sink, Sink)
        | layers_links
      ]

      {:ok, pipeline} = Pipeline.start_link(links: links)

      assert_pipeline_playback_changed(pipeline, :prepared, :playing)
      assert_start_of_stream(pipeline, :sink)

      Process.sleep(1000)

      for layer <- Enum.reverse(@layers) do
        # request encoding
        send_to_element(pipeline, :tee, {:select_encoding, {@endpoint_id, layer}})

        # see if Keyframe request is sent
        assert_sink_event(pipeline, {:source, layer}, %Membrane.KeyframeRequestEvent{})

        # Check if a buffer for a given layer actually arrives
        purge_all_buffer_reports(pipeline)
        assert_sink_buffer(pipeline, :sink, %Buffer{payload: fake_keyframe(^layer)})
      end

      Pipeline.terminate(pipeline, blocking: true)
    end
  end

  defp send_to_element(pipeline, element, msg) do
    Pipeline.execute_actions(pipeline, forward: {element, msg})
  end

  defp purge_all_buffer_reports(pipeline) do
    receive do
      {Membrane.Testing.Pipeline, ^pipeline, {:handle_notification, {{:buffer, _buffer}, :sink}}} ->
        purge_all_buffer_reports(pipeline)
    after
      0 -> :ok
    end
  end
end
