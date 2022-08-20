defmodule Membrane.RTC.Engine.WebRTC.TrackSenderTest do
  use ExUnit.Case, async: true

  import Membrane.Testing.Assertions

  alias Membrane.{Buffer, ParentSpec}
  alias Membrane.RTC.Engine.Track
  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender
  alias Membrane.Testing.{Pipeline, Sink, Source}

  @stream_id "stream1"
  @track_origin "generated"

  describe "TrackSender" do
    test "adds `is_keyframe` flag to each buffer" do
      [
        {Track.new(:video, @stream_id, @track_origin, :H264, 90_000, :raw, nil), false},
        {Track.new(:video, @stream_id, @track_origin, :VP8, 90_000, :raw, nil), false},
        {Track.new(:video, @stream_id, @track_origin, :OPUS, 48_000, :raw, nil), true}
      ]
      |> Enum.each(fn {track, expected_is_keyframe_value} ->
        test_is_keyframe(track, expected_is_keyframe_value)
      end)
    end
  end

  defp test_is_keyframe(track, expected_is_keyframe_value) do
    test_payload = <<1, 2, 3, 4, 5>>

    children = [
      source: %Source{caps: %Membrane.RTP{}, output: [test_payload]},
      track_sender: %TrackSender{track: track},
      sink: Sink
    ]

    links = ParentSpec.link_linear(children)
    {:ok, pipeline} = Pipeline.start_link(links: links)

    assert_sink_buffer(pipeline, :sink, %Buffer{
      payload: ^test_payload,
      metadata: %{is_keyframe: ^expected_is_keyframe_value}
    })

    Pipeline.terminate(pipeline, blocking?: true)
  end
end
