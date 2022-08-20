defmodule Membrane.RTC.Engine.WebRTC.TrackSenderTest do
  use ExUnit.Case, async: true

  import Membrane.ParentSpec
  import Membrane.Testing.Assertions

  require Membrane.Pad

  alias Membrane.{Buffer, Pad}
  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender
  alias Membrane.RTC.Engine.Track
  alias Membrane.Testing.{Pipeline, Sink, Source}

  @track_id "track1"
  @simulcast_encodings ["h", "m", "l"]
  @stream_id "stream1"
  @track_origin "generated"

  describe "TrackSender" do
    test "adds `is_keyframe` flag to each buffer" do
      [
        {Track.new(:video, @stream_id, @track_origin, :H264, 90_000, :raw, nil,
           id: @track_id,
           simulcast_encodings: @simulcast_encodings
         ), false},
        {Track.new(:video, @stream_id, @track_origin, :VP8, 90_000, :raw, nil,
           id: @track_id,
           simulcast_encodings: @simulcast_encodings
         ), false},
        {Track.new(:audio, @stream_id, @track_origin, :OPUS, 48_000, :raw, nil, id: @track_id),
         true}
      ]
      |> Enum.each(fn {track, expected_is_keyframe_value} ->
        test_is_keyframe(track, expected_is_keyframe_value)
      end)
    end
  end

  defp test_is_keyframe(%Track{type: :audio} = track, expected_is_keyframe_value) do
    test_payload = <<1, 2, 3, 4, 5>>

    children = [
      source: %Source{caps: %Membrane.RTP{}, output: [test_payload]},
      track_sender: %TrackSender{track: track},
      sink: Sink
    ]

    links = [
      link(:source)
      |> via_in(Pad.ref(:input, {@track_id, nil}))
      |> to(:track_sender)
      |> via_out(Pad.ref(:output, {@track_id, nil}))
      |> to(:sink)
    ]

    {:ok, pipeline} = Pipeline.start_link(children: children, links: links)

    assert_sink_buffer(pipeline, :sink, %Buffer{
      payload: ^test_payload,
      metadata: %{is_keyframe: ^expected_is_keyframe_value}
    })

    Pipeline.terminate(pipeline, blocking?: true)
  end

  defp test_is_keyframe(track, expected_is_keyframe_value) do
    test_payload = <<1, 2, 3, 4, 5>>

    children = [
      source_h: %Source{caps: %Membrane.RTP{}, output: [test_payload]},
      source_m: %Source{caps: %Membrane.RTP{}, output: [test_payload]},
      source_l: %Source{caps: %Membrane.RTP{}, output: [test_payload]},
      track_sender: %TrackSender{track: track},
      sink_h: Sink,
      sink_m: Sink,
      sink_l: Sink
    ]

    links = [
      link(:source_h)
      |> via_in(Pad.ref(:input, {@track_id, "h"}))
      |> to(:track_sender)
      |> via_out(Pad.ref(:output, {@track_id, "h"}))
      |> to(:sink_h),
      link(:source_m)
      |> via_in(Pad.ref(:input, {@track_id, "m"}))
      |> to(:track_sender)
      |> via_out(Pad.ref(:output, {@track_id, "m"}))
      |> to(:sink_m),
      link(:source_l)
      |> via_in(Pad.ref(:input, {@track_id, "l"}))
      |> to(:track_sender)
      |> via_out(Pad.ref(:output, {@track_id, "l"}))
      |> to(:sink_l)
    ]

    {:ok, pipeline} = Pipeline.start_link(children: children, links: links)

    assert_sink_buffer(pipeline, :sink_h, %Buffer{
      payload: ^test_payload,
      metadata: %{is_keyframe: ^expected_is_keyframe_value}
    })

    assert_sink_buffer(pipeline, :sink_m, %Buffer{
      payload: ^test_payload,
      metadata: %{is_keyframe: ^expected_is_keyframe_value}
    })

    assert_sink_buffer(pipeline, :sink_l, %Buffer{
      payload: ^test_payload,
      metadata: %{is_keyframe: ^expected_is_keyframe_value}
    })

    Pipeline.terminate(pipeline, blocking?: true)
  end
end
