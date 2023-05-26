defmodule Membrane.RTC.Engine.TeeTest do
  use ExUnit.Case, async: true

  import Membrane.Testing.Assertions
  import Membrane.ChildrenSpec

  require Membrane.Pad

  alias Membrane.{Buffer, Pad}
  alias Membrane.RTC.Engine.Tee

  alias Membrane.RTC.Engine.Event.{
    RequestTrackVariant,
    TrackVariantBitrate,
    TrackVariantPaused,
    TrackVariantResumed,
    TrackVariantSwitched,
    VoiceActivityChanged
  }

  alias Membrane.RTC.Engine.Exception.VoiceActivityError

  alias Membrane.RTC.Engine.Support.{TestSink, TestSource}
  alias Membrane.RTC.Engine.Track
  alias Membrane.Testing.Pipeline

  @endpoint_id "endpoint"
  @track_id "track1"
  @variants [:high, :medium, :low]
  @stream_id "stream1"
  @track_origin "generated"

  test "Tee sends TrackVariantResumed after linking output pad" do
    track = build_h264_track()
    pipeline = build_pipeline(track, [])

    Enum.each(track.variants, &mark_variant_as_resumed(pipeline, &1))

    Enum.each(track.variants, fn variant ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^variant})
    end)

    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :high})
    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :medium}, 0)
    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :low}, 0)

    spec =
      get_child(:tee)
      |> via_out(Pad.ref(:output, {:endpoint, :other_endpoint}))
      |> child(:other_sink, TestSink)

    Pipeline.execute_actions(pipeline, spec: spec)

    Enum.each(track.variants, fn variant ->
      assert_sink_event(pipeline, :other_sink, %TrackVariantResumed{variant: ^variant})
    end)

    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :high})
    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :medium}, 0)
    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :low}, 0)

    refute_sink_event(pipeline, :other_sink, %TrackVariantResumed{variant: :low})
    refute_sink_event(pipeline, :other_sink, %TrackVariantResumed{variant: :high}, 0)
    refute_sink_event(pipeline, :other_sink, %TrackVariantResumed{variant: :medium}, 0)

    :ok = Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee forwards TrackVariantBitrate events" do
    track = build_h264_track()
    pipeline = build_pipeline(track, [])

    new_variant_bitrates = %{high: 1300, medium: 800}

    events =
      Enum.map(new_variant_bitrates, fn {variant, bitrate} ->
        %TrackVariantBitrate{variant: variant, bitrate: bitrate}
      end)

    actions = Enum.flat_map(events, fn event -> [event: {:output, event}] end)

    Pipeline.execute_actions(pipeline,
      notify_child: {{:source, :high}, {:execute_actions, actions}}
    )

    Enum.each(new_variant_bitrates, fn {variant, bitrate} ->
      assert_sink_event(pipeline, :sink, %TrackVariantBitrate{
        variant: ^variant,
        bitrate: ^bitrate
      })
    end)

    :ok = Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee sends TrackVariantBitrate after linking output pad" do
    track = build_h264_track()
    pipeline = build_pipeline(track, [])

    new_variant = :high
    new_bitrate = 1488

    actions = [event: {:output, %TrackVariantBitrate{variant: new_variant, bitrate: new_bitrate}}]

    Pipeline.execute_actions(pipeline,
      notify_child: {{:source, new_variant}, {:execute_actions, actions}}
    )

    spec = [
      get_child(:tee)
      |> via_out(Pad.ref(:output, {:endpoint, :other_endpoint}))
      |> child(:other_sink, TestSink)
    ]

    Pipeline.execute_actions(pipeline, spec: spec)

    assert_sink_event(pipeline, :other_sink, %TrackVariantBitrate{
      variant: ^new_variant,
      bitrate: ^new_bitrate
    })

    :ok = Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee forwards TrackVariantPaused" do
    track = build_h264_track()
    pipeline = build_pipeline(track, [])

    mark_variant_as_paused(pipeline, :high)

    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :high})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :medium})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :low}, 0)

    mark_variant_as_paused(pipeline, :low)

    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :low})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :high})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :medium}, 0)

    :ok = Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee generates KeyframeRequestEvent on receiving RequestTrackVariant event" do
    track = build_h264_track()
    pipeline = build_pipeline(track, [])

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

    :ok = Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee raises on receiving invalid RequestTrackVariant event" do
    track = build_h264_track()
    pipeline = build_pipeline(track, [])

    Process.flag(:trap_exit, true)

    actions = [event: {:input, %RequestTrackVariant{variant: "invalid_track_variant"}}]
    Pipeline.execute_actions(pipeline, notify_child: {:sink, {:execute_actions, actions}})

    assert_receive {:EXIT, ^pipeline, {:shutdown, :child_crash}}
  end

  test "Tee correcly forwards variants" do
    # this test checks the following things:
    # * Tee doesn't send any variant until it is requested
    # * when variant is requested, Tee waits for a keyframe
    # for this variant
    # * when variant is marked as inactive and active again,
    # Tee doesn't send any variant until it's re-requested

    send_buffers = fn pipeline, variant ->
      [
        %Buffer{payload: <<1, 2, 3, 4, 5>>, metadata: %{is_keyframe: false}},
        %Buffer{payload: <<>>, metadata: %{is_keyframe: true}},
        %Buffer{payload: <<6, 7, 8, 9, 10>>, metadata: %{is_keyframe: false}}
      ]
      |> Enum.each(&send_buffer(pipeline, variant, &1))
    end

    request_and_check_high = fn pipeline ->
      request_track_variant(pipeline, :high)
      assert_sink_event(pipeline, {:source, :high}, %Membrane.KeyframeRequestEvent{})

      # check whether other variants are not forwarded to the sink
      refute_sink_buffer(pipeline, :sink, _buffer)

      # send three buffers, only the last two should be received in the sink
      send_buffers.(pipeline, :high)

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
    pipeline = build_pipeline(track, [], 3, false)

    Enum.each(track.variants, &mark_variant_as_resumed(pipeline, &1))

    Enum.each(track.variants, fn variant ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^variant})
    end)

    # we don't want to use generator for `:high`
    # to have full control over the number of buffers
    # being sent to the sink;
    # this breaks the concept of demands but it's only
    # for testing purposes

    send_buffers.(pipeline, :low)
    send_buffers.(pipeline, :medium)

    # tee shouldn't send us any packets
    # until we request specific track variant
    refute_sink_buffer(pipeline, :sink, _buffer)

    request_and_check_high.(pipeline)

    mark_variant_as_paused(pipeline, :high)
    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :high})

    mark_variant_as_resumed(pipeline, :high)
    assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: :high})

    refute_sink_buffer(pipeline, :sink, _buffer)

    request_and_check_high.(pipeline)

    :ok = Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee raises on receiving data from inactive track variant" do
    track = build_h264_track()
    pipeline = build_pipeline(track, [])

    mark_variant_as_paused(pipeline, :high)
    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: :high})

    Process.flag(:trap_exit, true)

    buffer = %Buffer{payload: <<1, 2, 3, 4, 5>>, metadata: %{is_keyframe: false}}
    send_buffer(pipeline, :high, buffer)

    assert_receive {:EXIT, ^pipeline, {:shutdown, :child_crash}}
  end

  test "Tee ignores KeyframeRequestEvent when there is no variant being forwarded" do
    track = build_h264_track()
    pipeline = build_pipeline(track, [])

    request_keyframe(pipeline)

    refute_sink_event(pipeline, {:source, :high}, %Membrane.KeyframeRequestEvent{})
    refute_sink_event(pipeline, {:source, :medium}, %Membrane.KeyframeRequestEvent{}, 0)
    refute_sink_event(pipeline, {:source, :low}, %Membrane.KeyframeRequestEvent{}, 0)

    :ok = Pipeline.terminate(pipeline, blocking?: true)
  end

  test "Tee forwards KeyframeRequestEvent when there is some variant being forwarded" do
    track = build_h264_track()
    pipeline = build_pipeline(track, [])

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

    :ok = Pipeline.terminate(pipeline, blocking?: true)
  end

  test "forwards VoiceActivityChanged" do
    track = build_opus_track()
    pipeline = build_pipeline(track, [])

    Enum.each(track.variants, &mark_variant_as_resumed(pipeline, &1))

    Enum.each(track.variants, fn variant ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^variant})
    end)

    request_track_variant(pipeline, :high)

    [
      %Buffer{payload: <<1, 2, 3, 4, 5>>, metadata: %{is_keyframe: false}},
      %Buffer{payload: <<>>, metadata: %{is_keyframe: true}}
    ]
    |> Enum.each(&send_buffer(pipeline, :high, &1))

    assert_sink_event(pipeline, :sink, %TrackVariantSwitched{new_variant: :high}, 5_000)

    event = %VoiceActivityChanged{voice_activity: :speech}

    Pipeline.execute_actions(pipeline,
      notify_child: {{:source, :high}, {:execute_actions, event: {:output, event}}}
    )

    # We're already on high, so we expect the event to be forwarded
    assert_sink_event(pipeline, :sink, ^event)

    :ok = Pipeline.terminate(pipeline, blocking?: true)
  end

  test "forwards VoiceActivityChanged after TrackVariantSwitched" do
    track = build_opus_track()
    pipeline = build_pipeline(track, [])

    Enum.each(track.variants, &mark_variant_as_resumed(pipeline, &1))

    Enum.each(track.variants, fn variant ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^variant})
    end)

    event = %VoiceActivityChanged{voice_activity: :speech}

    Pipeline.execute_actions(pipeline,
      notify_child: {{:source, :high}, {:execute_actions, event: {:output, event}}}
    )

    request_track_variant(pipeline, :high)

    # We shouldn't be getting an event before the switch actually happens
    refute_sink_event(pipeline, :sink, ^event)

    # Send the keyframe to switch
    buffer = %Buffer{payload: <<>>, metadata: %{is_keyframe: true}}
    send_buffer(pipeline, :high, buffer)

    # After we switch, we expect a VoiceActivityChanged event
    # right after TrackVariantSwitched
    # even though it was emitted a while ago
    assert_sink_event(pipeline, :sink, %TrackVariantSwitched{new_variant: :high})
    assert_sink_event(pipeline, :sink, ^event)
  end

  test "VoiceActivityError" do
    track = build_h264_track()

    assert_raise VoiceActivityError, fn ->
      Membrane.RTC.Engine.Tee.handle_event(
        Membrane.Pad.ref(:input, {0, :high}),
        %VoiceActivityChanged{voice_activity: :speech},
        nil,
        %{track: track}
      )
    end
  end

  defp build_h264_track() do
    Track.new(:video, @stream_id, @track_origin, :H264, 90_000, nil,
      id: @track_id,
      variants: @variants
    )
  end

  defp build_opus_track() do
    Track.new(:audio, @stream_id, @track_origin, :OPUS, 48_00, nil,
      id: @track_id,
      variants: [:high]
    )
  end

  defp build_pipeline(track, output, num_of_variants \\ 3, fast_start \\ true) do
    variants = Enum.take(track.variants, num_of_variants)

    pipeline = Pipeline.start_link_supervised!()

    tee_spec =
      child(:tee, %Tee{track: track})
      |> via_out(Pad.ref(:output, {:endpoint, @endpoint_id}))
      |> child(:sink, TestSink)

    variant_specs =
      for variant <- variants do
        source = %TestSource{
          stream_format: %Membrane.RTP{},
          output: output,
          fast_start: fast_start
        }

        child({:source, variant}, source)
        |> via_in(Pad.ref(:input, {track.id, variant}))
        |> get_child(:tee)
      end

    Pipeline.execute_actions(pipeline, spec: [tee_spec | variant_specs])

    for variant <- variants do
      assert_pipeline_notified(pipeline, {:source, variant}, :playing)
    end

    pipeline
  end

  defp mark_variant_as_resumed(pipeline, variant) do
    actions = [event: {:output, %TrackVariantResumed{variant: variant}}]

    Pipeline.execute_actions(pipeline,
      notify_child: {{:source, variant}, {:execute_actions, actions}}
    )
  end

  defp mark_variant_as_paused(pipeline, variant) do
    actions = [event: {:output, %TrackVariantPaused{variant: variant}}]

    Pipeline.execute_actions(pipeline,
      notify_child: {{:source, variant}, {:execute_actions, actions}}
    )
  end

  defp request_track_variant(pipeline, variant) do
    actions = [event: {:input, %RequestTrackVariant{variant: variant}}]
    Pipeline.execute_actions(pipeline, notify_child: {:sink, {:execute_actions, actions}})
  end

  defp send_buffer(pipeline, variant, buffer) do
    actions = [buffer: {:output, buffer}]

    Pipeline.execute_actions(pipeline,
      notify_child: {{:source, variant}, {:execute_actions, actions}}
    )
  end

  defp request_keyframe(pipeline) do
    actions = [event: {:input, %Membrane.KeyframeRequestEvent{}}]
    Pipeline.execute_actions(pipeline, notify_child: {:sink, {:execute_actions, actions}})
  end
end
