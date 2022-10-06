defmodule Membrane.RTC.Engine.WebRTC.TrackSenderTest do
  use ExUnit.Case, async: true

  import Membrane.ParentSpec
  import Membrane.Testing.Assertions

  require Membrane.Pad

  alias Membrane.{Buffer, Pad}
  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender
  alias Membrane.RTC.Engine.Event.{TrackVariantPaused, TrackVariantResumed}
  alias Membrane.RTC.Engine.Support.{TestSource, Utils}
  alias Membrane.RTC.Engine.Track
  alias Membrane.Testing.{Pipeline, Sink, Source}

  @track_id "track1"
  @variants [:high, :medium, :low]
  @stream_id "stream1"
  @track_origin "generated"

  describe "TrackSender" do
    test "adds `is_keyframe` flag to each buffer" do
      [
        {Track.new(:video, @stream_id, @track_origin, :H264, 90_000, nil,
           id: @track_id,
           variants: @variants
         ), false},
        {Track.new(:video, @stream_id, @track_origin, :VP8, 90_000, nil,
           id: @track_id,
           variants: @variants
         ), false},
        {Track.new(:audio, @stream_id, @track_origin, :OPUS, 48_000, nil, id: @track_id), true}
      ]
      |> Enum.each(fn {track, expected_is_keyframe_value} ->
        test_is_keyframe(track, expected_is_keyframe_value)
      end)
    end

    test "sends caps just once on given output pad" do
      track = build_h264_track()
      pipeline = build_video_pipeline(track, {nil, &Utils.generator/2}, 2)

      assert_sink_caps(pipeline, {:sink, :high}, %Membrane.RTP{})
      assert_sink_caps(pipeline, {:sink, :medium}, %Membrane.RTP{})

      refute_sink_caps(pipeline, {:sink, :high}, %Membrane.RTP{}, 0)
      refute_sink_caps(pipeline, {:sink, :medium}, %Membrane.RTP{}, 0)

      links = [
        link({:source, :low}, %Source{caps: %Membrane.RTP{}, output: []})
        |> via_in(Pad.ref(:input, {@track_id, :low}))
        |> to(:track_sender)
        |> via_out(Pad.ref(:output, {@track_id, :low}))
        |> to({:sink, :low}, Sink)
      ]

      actions = [{:spec, %Membrane.ParentSpec{links: links}}]
      Pipeline.execute_actions(pipeline, actions)

      assert_sink_caps(pipeline, {:sink, :low}, %Membrane.RTP{})

      refute_sink_caps(pipeline, {:sink, :high}, %Membrane.RTP{}, 0)
      refute_sink_caps(pipeline, {:sink, :medium}, %Membrane.RTP{}, 0)
      refute_sink_caps(pipeline, {:sink, :low}, %Membrane.RTP{}, 0)

      Pipeline.terminate(pipeline, blocking?: true)
    end

    test "sends TrackVariantResumed event when adding output pad" do
      track = build_h264_track()
      pipeline = build_video_pipeline(track, [], 2)

      assert_sink_event(pipeline, {:sink, :high}, %TrackVariantResumed{variant: :high})
      assert_sink_event(pipeline, {:sink, :medium}, %TrackVariantResumed{variant: :medium})

      # assert we receive just one TrackVariantResumed event
      refute_sink_event(pipeline, {:sink, :high}, %TrackVariantResumed{variant: :high})
      refute_sink_event(pipeline, {:sink, :medium}, %TrackVariantResumed{variant: :medium}, 0)

      links = [
        link({:source, :low}, %Source{caps: %Membrane.RTP{}, output: []})
        |> via_in(Pad.ref(:input, {@track_id, :low}))
        |> to(:track_sender)
        |> via_out(Pad.ref(:output, {@track_id, :low}))
        |> to({:sink, :low}, Sink)
      ]

      actions = [{:spec, %Membrane.ParentSpec{links: links}}]
      Pipeline.execute_actions(pipeline, actions)

      assert_sink_event(pipeline, {:sink, :low}, %TrackVariantResumed{variant: :low})
      refute_sink_event(pipeline, {:sink, :low}, %TrackVariantResumed{variant: :low})

      Pipeline.terminate(pipeline, blocking?: true)
    end

    test "sends TrackVariantPaused event when variant becomes inactive" do
      track = build_h264_track()
      pipeline = build_video_pipeline(track, {nil, &Utils.generator/2}, 3)

      Enum.each(@variants, fn variant ->
        Pipeline.execute_actions(pipeline, forward: {{:source, variant}, {:set_active, false}})
      end)

      Enum.each(@variants, fn variant ->
        assert_sink_event(pipeline, {:sink, variant}, %TrackVariantPaused{}, 3_000)
      end)

      Pipeline.terminate(pipeline, blocking?: true)
    end

    test "sends TrackVariantResumed event when variant becomes active" do
      track = build_h264_track()
      pipeline = build_video_pipeline(track, {nil, &Utils.generator/2}, 3)

      Enum.each(@variants, fn variant ->
        Pipeline.execute_actions(pipeline, forward: {{:source, variant}, {:set_active, false}})
      end)

      Enum.each(@variants, fn variant ->
        assert_sink_event(pipeline, {:sink, variant}, %TrackVariantPaused{}, 3_000)
      end)

      Enum.each(@variants, fn variant ->
        Pipeline.execute_actions(pipeline, forward: {{:source, variant}, {:set_active, true}})
      end)

      Enum.each(@variants, fn variant ->
        assert_sink_event(pipeline, {:sink, variant}, %TrackVariantResumed{}, 15_000)
      end)

      Pipeline.terminate(pipeline, blocking?: true)
    end
  end

  defp test_is_keyframe(%Track{type: :audio} = track, expected_is_keyframe_value) do
    test_payload = <<1, 2, 3, 4, 5>>

    pipeline = build_audio_pipeline(track, [test_payload])

    assert_sink_buffer(pipeline, :sink, %Buffer{
      payload: ^test_payload,
      metadata: %{is_keyframe: ^expected_is_keyframe_value}
    })

    Pipeline.terminate(pipeline, blocking?: true)
  end

  defp test_is_keyframe(track, expected_is_keyframe_value) do
    test_payload = <<1, 2, 3, 4, 5>>

    pipeline = build_video_pipeline(track, {nil, &Utils.generator/2})

    Enum.each(@variants, fn variant ->
      assert_sink_buffer(pipeline, {:sink, variant}, %Buffer{
        payload: ^test_payload,
        metadata: %{is_keyframe: ^expected_is_keyframe_value}
      })
    end)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  defp build_h264_track() do
    Track.new(:video, @stream_id, @track_origin, :H264, 90_000, nil,
      id: @track_id,
      variants: @variants
    )
  end

  defp build_audio_pipeline(track, source_buffers) do
    {:ok, pipeline} = Pipeline.start_link(links: [])
    assert_pipeline_playback_changed(pipeline, :prepared, :playing)

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

    actions = [spec: %Membrane.ParentSpec{children: children, links: links}]
    Pipeline.execute_actions(pipeline, actions)

    pipeline
  end

  defp build_video_pipeline(track, output, num_of_variants \\ 3) do
    variants = Enum.take(track.variants, num_of_variants)

    {:ok, pipeline} = Pipeline.start_link(links: [])
    assert_pipeline_playback_changed(pipeline, :prepared, :playing)

    variant_links =
      for variant <- variants do
        source = %TestSource{caps: %Membrane.RTP{}, output: output}

        link({:source, variant}, source)
        |> via_in(Pad.ref(:input, {@track_id, variant}))
        |> to(:track_sender)
      end

    track_sender_links =
      for variant <- variants do
        link(:track_sender)
        |> via_out(Pad.ref(:output, {@track_id, variant}))
        |> to({:sink, variant}, Sink)
      end

    actions = [
      spec: %Membrane.ParentSpec{
        children: [track_sender: %TrackSender{track: track}],
        links: variant_links ++ track_sender_links
      }
    ]

    Pipeline.execute_actions(pipeline, actions)

    pipeline
  end
end
