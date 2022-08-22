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

    test "sends caps just once on given output pad" do
      track =
        Track.new(:video, @stream_id, @track_origin, :H264, 90_000, :raw, nil,
          id: @track_id,
          simulcast_encodings: @simulcast_encodings
        )

      {:ok, pipeline} =
        build_video_pipeline(track, [])
        |> Pipeline.start_link()

      assert_sink_caps(pipeline, :sink_h, %Membrane.RTP{})
      assert_sink_caps(pipeline, :sink_m, %Membrane.RTP{})
      assert_sink_caps(pipeline, :sink_l, %Membrane.RTP{})

      refute_sink_caps(pipeline, :sink_h, %Membrane.RTP{}, 0)
      refute_sink_caps(pipeline, :sink_m, %Membrane.RTP{}, 0)
      refute_sink_caps(pipeline, :sink_l, %Membrane.RTP{}, 0)

      Pipeline.terminate(pipeline, blocking?: true)
    end
  end

  defp test_is_keyframe(%Track{type: :audio} = track, expected_is_keyframe_value) do
    test_payload = <<1, 2, 3, 4, 5>>

    {:ok, pipeline} =
      build_audio_pipeline(track, [test_payload])
      |> Pipeline.start_link()

    assert_sink_buffer(pipeline, :sink, %Buffer{
      payload: ^test_payload,
      metadata: %{is_keyframe: ^expected_is_keyframe_value}
    })

    Pipeline.terminate(pipeline, blocking?: true)
  end

  defp test_is_keyframe(track, expected_is_keyframe_value) do
    test_payload = <<1, 2, 3, 4, 5>>

    {:ok, pipeline} =
      build_video_pipeline(track, [test_payload])
      |> Pipeline.start_link()

    [:sink_h, :sink_m, :sink_l]
    |> Enum.each(fn sink ->
      assert_sink_buffer(pipeline, sink, %Buffer{
        payload: ^test_payload,
        metadata: %{is_keyframe: ^expected_is_keyframe_value}
      })
    end)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  defp build_audio_pipeline(track, source_buffers) do
    children = [
      source: %Source{caps: %Membrane.RTP{}, output: source_buffers},
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

    [children: children, links: links]
  end

  defp build_video_pipeline(track, source_buffers) do
    children = [
      source_m: %Source{caps: %Membrane.RTP{}, output: source_buffers},
      source_h: %Source{caps: %Membrane.RTP{}, output: source_buffers},
      source_l: %Source{caps: %Membrane.RTP{}, output: source_buffers},
      track_sender: %TrackSender{track: track},
      sink_h: Sink,
      sink_m: Sink,
      sink_l: Sink
    ]

    links =
      ["h", "m", "l"]
      |> Enum.map(encodings, fn encoding ->
        link(:"source_#{encoding}")
        |> via_in(Pad.ref(:input, {@track_id, encoding}))
        |> to(:track_sender)
        |> via_out(Pad.ref(:output, {@track_id, encoding}))
        |> to(:"sink_#{encoding}")
      end)

    [children: children, links: links]
  end
end
