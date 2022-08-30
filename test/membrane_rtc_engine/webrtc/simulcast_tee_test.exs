defmodule Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastTeeTest do
  use ExUnit.Case, async: true
  use Bitwise

  import Membrane.Testing.Assertions
  import Membrane.ParentSpec

  require Membrane.Pad

  alias Membrane.{Buffer, Pad}
  alias Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastTee

  alias Membrane.RTC.Engine.Event.{
    RequestTrackVariant,
    TrackVariantPaused,
    TrackVariantResumed
  }

  alias Membrane.RTC.Engine.Support.{TestSink, TestSource, Utils}
  alias Membrane.RTC.Engine.Track
  alias Membrane.Testing.{Pipeline, Source}

  @endpoint_id "endpoint"
  @track_id "track1"
  @simulcast_encodings ["h", "m", "l"]
  @stream_id "stream1"
  @track_origin "generated"

  test "SimulcastTee sends TrackVariantResumed after linking input pad" do
    track =
      Track.new(:video, @stream_id, @track_origin, :H264, 90_000, :raw, nil,
        id: @track_id,
        simulcast_encodings: @simulcast_encodings
      )

    pipeline = build_video_pipeline(track, {nil, &Utils.generator/2}, 2)

    Enum.each(["h", "m"], fn encoding ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^encoding})
    end)

    refute_sink_event(pipeline, :sink, _event, 0)
    refute_sink_buffer(pipeline, :sink, _buffer, 0)

    links = [
      link({:source, "l"}, %Source{caps: %Membrane.RTP{}, output: []})
      |> via_in(Pad.ref(:input, {@track_id, "l"}))
      |> to(:tee)
    ]

    actions = [{:spec, %Membrane.ParentSpec{links: links}}]
    Pipeline.execute_actions(pipeline, actions)

    assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: "l"})
    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: "m"}, 0)
    refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: "h"}, 0)
    refute_sink_buffer(pipeline, :sink, _buffer, 0)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  test "SimulcastTee sends TrackVariantResumed after linking output pad" do
    track =
      Track.new(:video, @stream_id, @track_origin, :H264, 90_000, :raw, nil,
        id: @track_id,
        simulcast_encodings: @simulcast_encodings
      )

    pipeline = build_video_pipeline(track, {nil, &Utils.generator/2})

    Enum.each(track.simulcast_encodings, fn encoding ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^encoding})
    end)

    Enum.each(track.simulcast_encodings, fn encoding ->
      refute_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^encoding}, 0)
    end)

    refute_sink_buffer(pipeline, :sink, _buffer, 0)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  test "SimulcastTee forwards TrackVariantPaused" do
    track =
      Track.new(:video, @stream_id, @track_origin, :H264, 90_000, :raw, nil,
        id: @track_id,
        simulcast_encodings: @simulcast_encodings
      )

    pipeline = build_video_pipeline(track, [])

    actions = [event: {:output, %TrackVariantPaused{variant: "h"}}]
    Pipeline.execute_actions(pipeline, forward: {{:source, "h"}, {:execute_actions, actions}})

    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: "h"})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: "m"})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: "l"}, 0)

    actions = [event: {:output, %TrackVariantPaused{variant: "l"}}]
    Pipeline.execute_actions(pipeline, forward: {{:source, "l"}, {:execute_actions, actions}})

    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: "l"})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: "h"})
    refute_sink_event(pipeline, :sink, %TrackVariantPaused{variant: "m"}, 0)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  test "SimulcastTee generates KeyframeRequestEvent on receiving RequestTrackVariant event" do
    track =
      Track.new(:video, @stream_id, @track_origin, :H264, 90_000, :raw, nil,
        id: @track_id,
        simulcast_encodings: @simulcast_encodings
      )

    pipeline = build_video_pipeline(track, [])

    actions = [event: {:input, %RequestTrackVariant{variant: "h"}}]
    Pipeline.execute_actions(pipeline, forward: {:sink, {:execute_actions, actions}})

    assert_sink_event(pipeline, {:source, "h"}, %Membrane.KeyframeRequestEvent{})
    # assert there is only on KeyframeRequestEvent for h
    refute_sink_event(pipeline, {:source, "h"}, %Membrane.KeyframeRequestEvent{})
    refute_sink_event(pipeline, {:source, "m"}, %Membrane.KeyframeRequestEvent{}, 0)
    refute_sink_event(pipeline, {:source, "l"}, %Membrane.KeyframeRequestEvent{}, 0)

    Pipeline.terminate(pipeline, blocking?: true)
  end

  @tag timeout: 1000
  test "SimulcastTee raises on receiving invalid RequestTrackVariant event" do
    track =
      Track.new(:video, @stream_id, @track_origin, :H264, 90_000, :raw, nil,
        id: @track_id,
        simulcast_encodings: @simulcast_encodings
      )

    pipeline = build_video_pipeline(track, [])

    Process.monitor(pipeline)

    actions = [event: {:input, %RequestTrackVariant{variant: "invalid_track_variant"}}]
    Pipeline.execute_actions(pipeline, forward: {:sink, {:execute_actions, actions}})

    receive do
      {:DOWN, _ref, :process, ^pipeline, {:shutdown, :child_crash}} -> :ok
    end
  end

  test "SimulcastTee doesn't send data until receiving RequestTrackVariant event and a keyframe for requested track variant" do
    track =
      Track.new(:video, @stream_id, @track_origin, :H264, 90_000, :raw, nil,
        id: @track_id,
        simulcast_encodings: @simulcast_encodings
      )

    pipeline = build_video_pipeline(track, {nil, &Utils.generator/2})

    Enum.each(track.simulcast_encodings, fn encoding ->
      assert_sink_event(pipeline, :sink, %TrackVariantResumed{variant: ^encoding})
    end)

    # tee shouldn't send us any packets
    # until we request specific track variant
    refute_sink_buffer(pipeline, :sink, _buffer)

    actions = [event: {:input, %RequestTrackVariant{variant: "h"}}]
    Pipeline.execute_actions(pipeline, forward: {:sink, {:execute_actions, actions}})
    assert_sink_event(pipeline, {:source, "h"}, %Membrane.KeyframeRequestEvent{})

    refute_sink_buffer(pipeline, :sink, _buffer)

    actions = [
      buffer: {:output, %Buffer{payload: <<>>, metadata: %{is_keyframe: true}}}
    ]

    Pipeline.execute_actions(pipeline, forward: {{:source, "h"}, {:execute_actions, actions}})

    # TODO assert we receive TrackVariantSwitched before any buffer

    # keyframe that we forced source to send
    assert_sink_buffer(pipeline, :sink, %Buffer{payload: <<>>, metadata: %{is_keyframe: true}})
    # some other buffer generated by source generator
    assert_sink_buffer(pipeline, :sink, %Buffer{
      payload: <<1, 2, 3, 4, 5>>,
      metadata: %{is_keyframe: false}
    })

    Pipeline.terminate(pipeline, blocking?: true)
  end

  @tag timeout: 1000
  test "SimulcastTee raises on receiving data from inactive track variant" do
    track =
      Track.new(:video, @stream_id, @track_origin, :H264, 90_000, :raw, nil,
        id: @track_id,
        simulcast_encodings: @simulcast_encodings
      )

    pipeline = build_video_pipeline(track, [])

    actions = [event: {:output, %TrackVariantPaused{variant: "h"}}]
    Pipeline.execute_actions(pipeline, forward: {{:source, "h"}, {:execute_actions, actions}})

    assert_sink_event(pipeline, :sink, %TrackVariantPaused{variant: "h"})

    Process.monitor(pipeline)

    actions = [
      buffer: {:output, %Buffer{payload: <<1, 2, 3, 4, 5>>, metadata: %{is_keyframe: false}}}
    ]

    Pipeline.execute_actions(pipeline, forward: {{:source, "h"}, {:execute_actions, actions}})

    receive do
      {:DOWN, _ref, :process, ^pipeline, {:shutdown, :child_crash}} -> :ok
    end
  end

  defp build_video_pipeline(track, output, num_of_encodings \\ 3) do
    encodings = Enum.take(track.simulcast_encodings, num_of_encodings)

    # TODO start/start_link?
    {:ok, pipeline} = Pipeline.start(links: [])

    assert_pipeline_playback_changed(pipeline, :prepared, :playing)

    encoding_links =
      for encoding <- encodings do
        source = %TestSource{caps: %Membrane.RTP{}, output: output}

        link({:source, encoding}, source)
        |> via_in(Pad.ref(:input, {track.id, encoding}))
        |> to(:tee)
      end

    tee_link =
      link(:tee)
      |> via_out(Pad.ref(:output, {:endpoint, @endpoint_id}))
      |> to(:sink, TestSink)

    actions = [
      spec: %Membrane.ParentSpec{
        children: [tee: %SimulcastTee{track: track}],
        links: encoding_links ++ [tee_link]
      }
    ]

    Pipeline.execute_actions(pipeline, actions)
    pipeline
  end
end
