defmodule Membrane.RTC.Engine.TeeTest do
  use ExUnit.Case, async: true

  import Membrane.Testing.Assertions
  import Membrane.ParentSpec

  require Membrane.Pad

  alias Membrane.{Buffer, Pad}
  alias Membrane.RTC.Engine.Tee

  alias Membrane.RTC.Engine.Event.{
    RequestTrackVariant,
    TrackVariantPaused,
    TrackVariantResumed
  }

  alias Membrane.RTC.Engine.Support.{TestSink, TestSource, Utils}
  alias Membrane.RTC.Engine.Track
  alias Membrane.Testing.Pipeline

  @endpoint_id "endpoint"
  @track_id "track1"
  @variants [:high, :medium, :low]
  @stream_id "stream1"
  @track_origin "generated"

  test "Tee sends TrackVariantResumed after linking output pad" do
    track = build_h264_track()
    pipeline = build_video_pipeline(track, [])

    Enum.each(track.variants, &mark_variant_as_resumed(pipeline, &1))

    Enum.each(track.variants, fn variant ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^variant})
    end)

    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :high})
    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :medium}, 0)
    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :low}, 0)

    links = [
      link(:tee)
      |> via_out(Pad.ref(:output, {:endpoint, :other_endpoint}))
      |> to(:other_sink, TestSink)
    ]

    actions = [spec: %Membrane.ParentSpec{links: links}]
    Pipeline.execute_actions(pipeline, actions)

    Enum.each(track.variants, fn variant ->
      assert_sink_event(pipeline, :other_sink, %TrackVariantResumed{variant: ^variant})
    end)

    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :high})
    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :medium}, 0)
    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :low}, 0)

    refute_sink_event(pipeline, :other_sink, %TrackVariantResumed{variant: :low})
    refute_sink_event(pipeline, :other_sink, %TrackVariantResumed{variant: :high}, 0)
    refute_sink_event(pipeline, :other_sink, %TrackVariantResumed{variant: :medium}, 0)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee forwards TrackVariantPaused" do
    track = build_h264_track()
    pipeline = build_video_pipeline(track, [])

    mark_variant_as_paused(pipeline, :high)

    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :high})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :medium})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :low}, 0)

    mark_variant_as_paused(pipeline, :low)

    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :low})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :high})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :medium}, 0)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee generates KeyframeRequestEvent on receiving RequestTrackVariant event" do
    track = build_h264_track()
    pipeline = build_video_pipeline(track, [])

    Enum.each(track.variants, &mark_variant_as_resumed(pipeline, &1))

    # wait until TrackVariantResumed is received in tee
    # (we have to assert sink in fact)
    Enum.each(track.variants, fn variant ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^variant})
    end)

    request_track_variant(pipeline, :high)

    assert_sink_event(pipeline, {:source, :high}, %Membrane.KeyframeRequestEvent{})
    # assert there is only one KeyframeRequestEvent for h
    refute_sink_event(pipeline, {:source, :high}, %Membrane.KeyframeRequestEvent{})
    refute_sink_event(pipeline, {:source, :medium}, %Membrane.KeyframeRequestEvent{}, 0)
    refute_sink_event(pipeline, {:source, :low}, %Membrane.KeyframeRequestEvent{}, 0)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee raises on receiving invalid RequestTrackVariant event" do
    track = build_h264_track()
    pipeline = build_video_pipeline(track, [])

    Process.monitor(pipeline)

    actions = [event: {:input, %RequestTrackVariant{variant: "invalid_track_variant"}}]
    Pipeline.execute_actions(pipeline, forward: {:sink, {:execute_actions, actions}})

    assert_receive {:DOWN, _ref, :process, ^pipeline, {:shutdown, :child_crash}}
  end

  test "Tee correcly forwards variants" do
    # this test checks the following things:
    # * Tee doesn't send any variant until it is requested
    # * when variant is requested, Tee waits for a keyframe
    # for this variant
    # * when variant is marked as inactive and active again,
    # Tee doesn't send any variant until it's re-requested

    request_and_check_high = fn pipeline ->
      request_track_variant(pipeline, :high)
      assert_sink_event(pipeline, {:source, :high}, %Membrane.KeyframeRequestEvent{})

      # check whether other variants are not forwarded to the sink
      refute_sink_buffer(pipeline, :sink, _buffer)

      # send three buffers, only the last two should be received in the sink
      [
        %Buffer{payload: <<1, 2, 3, 4, 5>>, metadata: %{is_keyframe: false}},
        %Buffer{payload: <<>>, metadata: %{is_keyframe: true}},
        %Buffer{payload: <<6, 7, 8, 9, 10>>, metadata: %{is_keyframe: false}}
      ]
      |> Enum.each(&send_buffer(pipeline, :high, &1))

      # TODO assert we receive TrackVariantSwitched before any buffer

      # keyframe that we forced source to send
      assert_sink_buffer(pipeline, :sink, %Buffer{payload: <<>>, metadata: %{is_keyframe: true}})
      # the next buffer after the keyframe we forced source to send
      assert_sink_buffer(pipeline, :sink, %Buffer{
        payload: <<6, 7, 8, 9, 10>>,
        metadata: %{is_keyframe: false}
      })
    end

    track = build_h264_track()
    pipeline = build_video_pipeline(track, {nil, &Utils.generator/2}, 3, false)

    Enum.each(track.variants, &mark_variant_as_resumed(pipeline, &1))

    Enum.each(track.variants, fn variant ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^variant})
    end)

    # we don't want to use generator for `:high`
    # to have full control over the number of buffers
    # being sent to the sink;
    # this breaks the concept of demands but it's only
    # for testing purposes
    activate_source(pipeline, :low)
    activate_source(pipeline, :medium)

    # tee shouldn't send us any packets
    # until we request specific track variant
    refute_sink_buffer(pipeline, :sink, _buffer)

    request_and_check_high.(pipeline)

    mark_variant_as_paused(pipeline, :high)
    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :high})

    mark_variant_as_resumed(pipeline, :high)
    assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :high})

    activate_source(pipeline, :high)

    refute_sink_buffer(pipeline, :sink, _buffer)

    request_and_check_high.(pipeline)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee raises on receiving data from inactive track variant" do
    track = build_h264_track()
    pipeline = build_video_pipeline(track, [])

    mark_variant_as_paused(pipeline, :high)
    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :high})

    Process.monitor(pipeline)

    buffer = %Buffer{payload: <<1, 2, 3, 4, 5>>, metadata: %{is_keyframe: false}}
    send_buffer(pipeline, :high, buffer)

    assert_receive {:DOWN, _ref, :process, ^pipeline, {:shutdown, :child_crash}}
  end

  test "Tee ignores KeyframeRequestEvent when there is no variant being forwarded" do
    track = build_h264_track()
    pipeline = build_video_pipeline(track, [])

    request_keyframe(pipeline)

    refute_sink_event(pipeline, {:source, :high}, %Membrane.KeyframeRequestEvent{})
    refute_sink_event(pipeline, {:source, :medium}, %Membrane.KeyframeRequestEvent{}, 0)
    refute_sink_event(pipeline, {:source, :low}, %Membrane.KeyframeRequestEvent{}, 0)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee forwards KeyframeRequestEvent when there is some variant being forwarded" do
    track = build_h264_track()
    pipeline = build_video_pipeline(track, [])

    Enum.each(track.variants, &mark_variant_as_resumed(pipeline, &1))

    Enum.each(track.variants, fn variant ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^variant})
    end)

    request_track_variant(pipeline, :high)

    buffer = %Buffer{payload: <<>>, metadata: %{is_keyframe: true}}
    send_buffer(pipeline, :high, buffer)

    request_keyframe(pipeline)

    assert_sink_event(pipeline, {:source, :high}, %Membrane.KeyframeRequestEvent{})
    refute_sink_event(pipeline, {:source, :medium}, %Membrane.KeyframeRequestEvent{})
    refute_sink_event(pipeline, {:source, :low}, %Membrane.KeyframeRequestEvent{}, 0)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  defp build_h264_track() do
    Track.new(:video, @stream_id, @track_origin, :H264, 90_000, nil,
      id: @track_id,
      variants: @variants
    )
  end

  defp build_video_pipeline(track, output, num_of_variants \\ 3, fast_start \\ true) do
    variants = Enum.take(track.variants, num_of_variants)

    # TODO start/start_link?
    {:ok, pipeline} = Pipeline.start(links: [])

    assert_pipeline_playback_changed(pipeline, :prepared, :playing)

    variant_links =
      for variant <- variants do
        source = %TestSource{caps: %Membrane.RTP{}, output: output, fast_start: fast_start}

        link({:source, variant}, source)
        |> via_in(Pad.ref(:input, {track.id, variant}))
        |> to(:tee)
      end

    tee_link =
      link(:tee)
      |> via_out(Pad.ref(:output, {:endpoint, @endpoint_id}))
      |> to(:sink, TestSink)

    actions = [
      spec: %Membrane.ParentSpec{
        children: [tee: %Tee{track: track}],
        links: variant_links ++ [tee_link]
      }
    ]

    Pipeline.execute_actions(pipeline, actions)
    pipeline
  end

  defp mark_variant_as_resumed(pipeline, variant) do
    actions = [event: {:output, %TrackVariantResumed{variant: variant}}]
    Pipeline.execute_actions(pipeline, forward: {{:source, variant}, {:execute_actions, actions}})
  end

  defp mark_variant_as_paused(pipeline, variant) do
    actions = [event: {:output, %TrackVariantPaused{variant: variant}}]
    Pipeline.execute_actions(pipeline, forward: {{:source, variant}, {:execute_actions, actions}})
  end

  defp request_track_variant(pipeline, variant) do
    actions = [event: {:input, %RequestTrackVariant{variant: variant}}]
    Pipeline.execute_actions(pipeline, forward: {:sink, {:execute_actions, actions}})
  end

  defp send_buffer(pipeline, variant, buffer) do
    actions = [buffer: {:output, buffer}]
    Pipeline.execute_actions(pipeline, forward: {{:source, variant}, {:execute_actions, actions}})
  end

  defp activate_source(pipeline, variant) do
    Pipeline.execute_actions(pipeline, forward: {{:source, variant}, {:set_active, true}})
  end

  defp request_keyframe(pipeline) do
    actions = [event: {:input, %Membrane.KeyframeRequestEvent{}}]
    Pipeline.execute_actions(pipeline, forward: {:sink, {:execute_actions, actions}})
  end
end
