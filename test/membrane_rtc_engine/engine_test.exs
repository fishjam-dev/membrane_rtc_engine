defmodule Membrane.RTC.EngineTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.{Endpoint, Track}

  alias Membrane.RTC.Engine.Support.TestEndpoint

  setup do
    options = [
      id: "test_rtc"
    ]

    {:ok, pid} = Engine.start_link(options, [])

    Engine.register(pid, self())

    on_exit(fn -> assert :ok = Engine.terminate(pid, blocking?: true) end)

    [rtc_engine: pid]
  end

  describe ":ready message" do
    test "triggers :new_endpoint", %{rtc_engine: rtc_engine} do
      endpoint_spec = %TestEndpoint{rtc_engine: rtc_engine, owner: self()}
      first_endpoint = "endpoint1"
      second_endpoint = "endpoint2"

      Engine.add_endpoint(rtc_engine, endpoint_spec, id: first_endpoint)
      # make first endpoint ready so it can receive notification about new endpoints
      Engine.message_endpoint(
        rtc_engine,
        first_endpoint,
        {:execute_actions, [notify_parent: {:ready, nil}]}
      )

      Engine.add_endpoint(rtc_engine, endpoint_spec, id: second_endpoint)

      Engine.message_endpoint(
        rtc_engine,
        second_endpoint,
        {:execute_actions, [notify_parent: {:ready, "metadata"}]}
      )

      assert_receive {:new_endpoint, %Endpoint{id: ^second_endpoint, metadata: "metadata"}}
      assert_receive {:ready, []}
      refute_receive {:new_tracks, []}
    end

    test "reports other endpoints", %{rtc_engine: rtc_engine} do
      endpoint1_spec = %TestEndpoint{rtc_engine: rtc_engine, owner: self()}
      endpoint1_track = video_track("endpoint1", "track1", "track1-metadata", "stream1")
      endpoint2_spec = %TestEndpoint{rtc_engine: rtc_engine, owner: self()}

      Engine.add_endpoint(rtc_engine, endpoint1_spec, id: "endpoint1")

      Engine.message_endpoint(
        rtc_engine,
        "endpoint1",
        {:execute_actions,
         [
           notify_parent: {:ready, "endpoint1-metadata"},
           notify_parent: {:publish, {:new_tracks, [endpoint1_track]}}
         ]}
      )

      assert_receive {:ready, []}

      Engine.add_endpoint(rtc_engine, endpoint2_spec, id: "endpoint2")

      Engine.message_endpoint(
        rtc_engine,
        "endpoint2",
        {:execute_actions, [notify_parent: {:ready, "endpoint2-metadata"}]}
      )

      assert_receive {:ready, endpoints_in_room}

      assert [
               %Endpoint{
                 id: "endpoint1",
                 metadata: "endpoint1-metadata",
                 type: TestEndpoint,
                 inbound_tracks: %{
                   "track1" => %Track{
                     id: "track1",
                     origin: "endpoint1",
                     metadata: "track1-metadata"
                   }
                 }
               }
             ] = endpoints_in_room

      assert_receive {:new_tracks, [%Track{id: "track1"}]}
    end
  end

  describe ":update_track_metadata" do
    setup :setup_for_metadata_tests

    test "triggers :track_metadata_updated", %{
      rtc_engine: rtc_engine,
      track: %Track{id: track_id},
      endpoint: %{id: endpoint_id}
    } do
      Engine.message_endpoint(
        rtc_engine,
        endpoint_id,
        {:execute_actions, [notify_parent: {:update_track_metadata, track_id, "new-metadata"}]}
      )

      assert_receive {:track_metadata_updated, %Track{id: ^track_id, metadata: "new-metadata"}}
    end

    test "ignores identical metadata", %{
      rtc_engine: rtc_engine,
      track: track,
      endpoint: %{id: endpoint_id}
    } do
      Engine.message_endpoint(
        rtc_engine,
        endpoint_id,
        {:execute_actions, [notify_parent: {:update_track_metadata, track.id, track.metadata}]}
      )

      refute_receive {:track_metadata_updated, _track}
    end
  end

  describe ":update_endpoint_metadata" do
    setup :setup_for_metadata_tests

    test "triggers :endpoint_metadata_updated", %{
      rtc_engine: rtc_engine,
      endpoint: %{id: endpoint_id}
    } do
      Engine.message_endpoint(
        rtc_engine,
        endpoint_id,
        {:execute_actions, [notify_parent: {:update_endpoint_metadata, "new-metadata"}]}
      )

      assert_receive {:endpoint_metadata_updated,
                      %Endpoint{id: ^endpoint_id, metadata: "new-metadata"}}
    end

    test "ignores identical metadata", %{rtc_engine: rtc_engine, endpoint: endpoint} do
      Engine.message_endpoint(
        rtc_engine,
        endpoint.id,
        {:execute_actions, [notify_parent: {:update_endpoint_metadata, endpoint.metadata}]}
      )

      refute_receive {:endpoint_metadata_updated, _track}
    end
  end

  describe "Engine.message_endpoint/3" do
    test "forwards message to endpoint", %{rtc_engine: rtc_engine} do
      endpoint = %TestEndpoint{rtc_engine: rtc_engine, owner: self()}
      endpoint_id = :test_endpoint
      :ok = Engine.add_endpoint(rtc_engine, endpoint, id: endpoint_id)
      :ok = Engine.message_endpoint(rtc_engine, endpoint_id, :message)
      assert_receive(:message, 1_000)
    end

    test "does nothing when endpoint doesn't exist", %{rtc_engine: rtc_engine} do
      endpoint_id = :test_endpoint
      :ok = Engine.message_endpoint(rtc_engine, endpoint_id, :message)
      refute_receive :message
    end
  end

  describe "Engine.get_endpoints/2" do
    test "get list of endpoints", %{rtc_engine: rtc_engine} do
      endpoint = %TestEndpoint{rtc_engine: rtc_engine, owner: self()}
      endpoint_id = :test_endpoint
      :ok = Engine.add_endpoint(rtc_engine, endpoint, id: endpoint_id)
      endpoints = Engine.get_endpoints(rtc_engine)
      assert [%{id: ^endpoint_id, type: TestEndpoint}] = endpoints
    end
  end

  test "engine sends EndpointCrashed message", %{rtc_engine: rtc_engine} do
    endpoint = %TestEndpoint{rtc_engine: rtc_engine, owner: self()}
    endpoint_id = :test_endpoint
    :ok = Engine.add_endpoint(rtc_engine, endpoint, id: endpoint_id)
    msg = {:execute_actions, [:some_invalid_action]}
    :ok = Engine.message_endpoint(rtc_engine, :test_endpoint, msg)
    assert_receive %Membrane.RTC.Engine.Message.EndpointCrashed{endpoint_id: :test_endpoint}
  end

  defp video_track(endpoint_id, track_id, metadata, stream_id \\ "test-stream") do
    Engine.Track.new(:video, stream_id, endpoint_id, :VP8, nil, nil,
      id: track_id,
      metadata: metadata
    )
  end

  defp setup_for_metadata_tests(%{rtc_engine: rtc_engine}) do
    track = video_track("track-endpoint", "track1", "track-metadata")

    endpoint = %Endpoint{
      id: "track-endpoint",
      type: Endpoint.WebRTC,
      metadata: "original-metadata"
    }

    track_endpoint = %TestEndpoint{rtc_engine: rtc_engine}

    server_endpoint = %TestEndpoint{
      rtc_engine: rtc_engine,
      owner: self()
    }

    Engine.add_endpoint(rtc_engine, track_endpoint, id: endpoint.id)

    Engine.message_endpoint(
      rtc_engine,
      endpoint.id,
      {:execute_actions,
       notify_parent: {:ready, endpoint.metadata},
       notify_parent: {:publish, {:new_tracks, [track]}}}
    )

    Engine.add_endpoint(rtc_engine, server_endpoint, id: "server-endpoint")

    Engine.message_endpoint(
      rtc_engine,
      "server-endpoint",
      {:execute_actions, [notify_parent: {:ready, nil}]}
    )

    assert_receive {:new_tracks, [%Track{id: "track1"}]}
    assert :ok = Engine.subscribe(rtc_engine, "server-endpoint", "track1")

    Engine.message_endpoint(
      rtc_engine,
      endpoint.id,
      {:execute_actions,
       [notify_parent: {:track_ready, track.id, hd(track.variants), track.encoding}]}
    )

    [
      track: track,
      track_endpoint: track_endpoint,
      endpoint: endpoint
    ]
  end
end
