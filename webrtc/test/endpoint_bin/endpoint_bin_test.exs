defmodule Membrane.WebRTC.EndpointBinTest do
  use ExUnit.Case, async: true

  import Membrane.ChildrenSpec

  alias Membrane.WebRTC.EndpointBin
  alias Membrane.WebRTC.Extension.{Mid, Rid}

  alias Membrane.RTC.Engine.Support.Utils

  @directions [:sendonly, :recvonly, :sendrecv]

  Enum.map(@directions, fn direction ->
    test "creating #{inspect(direction)} EndpointBin" do
      pipeline =
        Membrane.Testing.Pipeline.start_link_supervised!(
          spec: child(:endpoint, %EndpointBin{direction: unquote(direction)})
        )

      Membrane.Testing.Pipeline.terminate(pipeline)
    end
  end)

  test "adding outbound tracks to :recvonly EndpointBin causes raise" do
    options = %EndpointBin{direction: :recvonly}
    {_spec, state} = EndpointBin.handle_init(nil, options)
    track = Utils.get_track()

    assert_raise RuntimeError, fn ->
      EndpointBin.handle_parent_notification({:add_tracks, [track]}, nil, state)
    end
  end

  Enum.map([:sendonly, :sendrecv], fn direction ->
    test "adding outbound tracks to #{inspect(direction)} EndpointBin passes" do
      options = %EndpointBin{direction: unquote(direction)}
      {_spec, state} = EndpointBin.handle_init(nil, options)
      track = Utils.get_track()
      assert EndpointBin.handle_parent_notification({:add_tracks, [track]}, nil, state)
    end
  end)

  test "sendonly EndpointBin rejects offer with incoming tracks" do
    test_generating_proper_sdp_answer(:sendonly, :inactive)
  end

  Enum.map([:recvonly, :sendrecv], fn direction ->
    test "#{inspect(direction)} EndpointBin accepts offer with incoming tracks" do
      test_generating_proper_sdp_answer(unquote(direction), :recvonly)
    end
  end)

  test "EndpointBin raises when peer offers outbound tracks on its own" do
    offer = File.read!("test/fixtures/2_outgoing_tracks_sdp.txt")
    sdp_offer_msg = {:signal, {:sdp_offer, offer, %{}}}
    handshake_init_data_not = {:handshake_init_data, 1, <<>>}
    options = %EndpointBin{direction: :sendrecv}

    {_spec, state} = EndpointBin.handle_init(nil, options)
    {[], state} = EndpointBin.handle_child_notification(handshake_init_data_not, nil, nil, state)

    assert_raise RuntimeError,
                 "Received new outbound tracks in SDP offer which is not allowed.",
                 fn -> EndpointBin.handle_parent_notification(sdp_offer_msg, nil, state) end
  end

  test "sendonly EndpointBin raises when receives RTP stream" do
    options = %EndpointBin{direction: :sendonly}
    {_spec, state} = EndpointBin.handle_init(nil, options)

    assert_raise RuntimeError,
                 ~r/Received new RTP stream but EndpointBin is set to :sendonly./,
                 fn ->
                   EndpointBin.handle_child_notification(
                     {:new_rtp_stream, 1234, 96, []},
                     nil,
                     nil,
                     state
                   )
                 end
  end

  defp test_generating_proper_sdp_answer(endpoint_bin_direction, expected_media_direction) do
    # this function creates EndpointBin with direction set to `endpoint_bin_direction`,
    # applies SDP offer and checks correctness of generated SDP answer by asserting
    # against media direction of each m-line i.e. whether it is set to `expected_media_direction`
    offer = File.read!("test/fixtures/2_incoming_tracks_sdp.txt")

    mid_to_track_id = %{
      "0" => "9e9fea81-0f48-4992-a5c2-3e475ca9d5fe:030b4e10-3dc0-4fa5-bf7a-f50f7237ef79",
      "1" => "9e9fea81-0f48-4992-a5c2-3e475ca9d5fe:ea223f51-c234-4431-8218-61b940c7d415"
    }

    fingerprint = Utils.get_cert_fingerprint()

    sdp_offer_msg = {:signal, {:sdp_offer, offer, mid_to_track_id}}
    handshake_init_data_not = {:handshake_init_data, 1, fingerprint}

    options = %EndpointBin{direction: endpoint_bin_direction, extensions: [Mid, Rid]}
    {_spec, state} = EndpointBin.handle_init(nil, options)
    {[], state} = EndpointBin.handle_child_notification(handshake_init_data_not, nil, nil, state)
    {actions, _state} = EndpointBin.handle_parent_notification(sdp_offer_msg, nil, state)

    assert [answer] =
             Enum.flat_map(actions, fn
               {:notify_parent, {:signal, {:sdp_answer, answer, _mid_to_track_id}}} -> [answer]
               _action -> []
             end)

    ExSDP.parse!(answer)
    |> then(& &1.media)
    |> Enum.each(fn media ->
      assert ExSDP.Media.get_attribute(media, expected_media_direction) ==
               expected_media_direction
    end)
  end
end
