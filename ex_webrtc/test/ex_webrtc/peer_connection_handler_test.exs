defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC.PeerConnectionHandlerTest do
  use ExUnit.Case, async: true

  import Membrane.ChildrenSpec
  import Membrane.Testing.Assertions

  alias ExWebRTC.{PeerConnection, SessionDescription}

  alias Membrane.Testing.Pipeline

  alias Membrane.RTC.Engine.Endpoint.ExWebRTC.PeerConnectionHandler
  alias Membrane.RTC.Engine.Track

  @endpoint_id "ex_webrtc_endpoint"

  setup do
    {:ok, pc} = PeerConnection.start_link()

    pipeline = Pipeline.start_link_supervised!(spec: get_pc_handler())

    %{pc: pc, pipeline: pipeline}
  end

  test "peer adds single track", %{pc: pc, pipeline: pipeline} do
    {:ok, _transceiver} = PeerConnection.add_transceiver(pc, :video, direction: :sendonly)
    {:ok, offer} = PeerConnection.create_offer(pc)
    :ok = PeerConnection.set_local_description(pc, offer)

    media_event = %{"sdpOffer" => SessionDescription.to_json(offer), "midToTrackId" => %{}}

    outbound_tracks = %{}
    Pipeline.notify_child(pipeline, :handler, {:offer, media_event, outbound_tracks})

    assert_pipeline_notified(
      pipeline,
      :handler,
      {:answer, %{"type" => "answer", "sdp" => _sdp} = answer, _new_mid_to_track_id}
    )

    assert_pipeline_notified(pipeline, :handler, {:tracks, tracks})

    assert length(tracks) == 1
    track = List.first(tracks)
    assert %{type: :video, origin: @endpoint_id, encoding: :VP8, variants: [:high]} = track

    answer = SessionDescription.from_json(answer)
    PeerConnection.set_remote_description(pc, answer)

    assert_pipeline_notified(pipeline, :handler, :negotiation_done)

    [transceiver] = PeerConnection.get_transceivers(pc)
    assert transceiver.current_direction == :sendonly
  end

  test "connection handler adds single track", %{pc: pc, pipeline: pipeline} do
    {:ok, _transceiver} = PeerConnection.add_transceiver(pc, :video, direction: :recvonly)
    {:ok, offer} = PeerConnection.create_offer(pc)
    :ok = PeerConnection.set_local_description(pc, offer)

    media_event = %{"sdpOffer" => SessionDescription.to_json(offer), "midToTrackId" => %{}}

    track = engine_video_track()
    outbound_tracks = %{track.id => track}
    Pipeline.notify_child(pipeline, :handler, {:offer, media_event, outbound_tracks})

    assert_pipeline_notified(
      pipeline,
      :handler,
      {:answer, %{"type" => "answer", "sdp" => _sdp} = answer, new_mid_to_track_id}
    )

    refute_pipeline_notified(pipeline, :handler, {:tracks, _tracks})

    answer = SessionDescription.from_json(answer)
    PeerConnection.set_remote_description(pc, answer)

    assert_pipeline_notified(pipeline, :handler, :negotiation_done)

    [{track_mid, track_id}] = Map.to_list(new_mid_to_track_id)

    [transceiver] = PeerConnection.get_transceivers(pc)
    assert transceiver.current_direction == :recvonly
    assert transceiver.mid == track_mid
    assert track_id == track.id
  end

  defp get_pc_handler() do
    [
      child(:handler, %PeerConnectionHandler{
        endpoint_id: @endpoint_id,
        ice_port_range: 50_000..50_050
      })
    ]
  end

  defp engine_video_track() do
    codec =
      PeerConnection.Configuration.default_video_codecs()
      |> Enum.filter(&(&1.mime_type == "video/VP8"))
      |> List.first()

    Track.new(
      :video,
      Track.stream_id(),
      @endpoint_id,
      :VP8,
      codec.clock_rate,
      codec.sdp_fmtp_line
    )
  end
end
