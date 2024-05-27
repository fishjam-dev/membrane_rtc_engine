defmodule Membrane.RTC.Engine.Endpoint.Recording.EdgeTimestampSaverTest do
  use ExUnit.Case, async: true

  import Membrane.Testing.Assertions
  import Membrane.ChildrenSpec
  import MockTrack

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Endpoint.Recording.EdgeTimestampSaver
  alias Membrane.RTCPEvent
  alias Membrane.Testing.{Pipeline, Sink, Source}

  tests_data = [
    %{
      title: "happy path",
      timestamps: Enum.to_list(1..200),
      rtcp_events: 5,
      end_timestamps: [101, 200]
    },
    %{
      title: "edge timestamp not in order",
      timestamps: Enum.to_list(1..98) ++ [100, 99] ++ Enum.to_list(101..200),
      rtcp_events: 5,
      end_timestamps: [101, 200]
    },
    %{
      title: "Long sequence not in order",
      timestamps: Enum.to_list(1..50) ++ Enum.to_list(100..10) ++ Enum.to_list(101..150),
      rtcp_events: 5,
      end_timestamps: [101, 150]
    },
    %{
      title: "End is not in order",
      timestamps: Enum.to_list(1..50) ++ Enum.to_list(70..50),
      rtcp_events: 5,
      end_timestamps: [70]
    },
    %{
      title: "RTCP reports, before buffers",
      timestamps: [],
      rtcp_events: 5,
      end_timestamps: []
    }
  ]

  for test_data <- tests_data do
    test test_data.title do
      pipeline = Pipeline.start_link_supervised!()
      timestamps = unquote(test_data.timestamps)
      rtcp_events = unquote(test_data.rtcp_events)

      buffers =
        for i <- timestamps do
          {:buffer, {:output, %Buffer{payload: <<>>, metadata: %{rtp: %{timestamp: i}}}}}
        end

      events =
        for idx <- 0..rtcp_events do
          {:event, {:output, %RTCPEvent{rtcp: create_rtcp_report(1, 1, idx)}}}
        end

      actions = buffers ++ events ++ [{:end_of_stream, :output}]
      generator = fn state, _size -> {actions, state} end

      spec =
        child(:source, %Source{output: {nil, generator}})
        |> child({:edge_timestamp_saver, :track_id}, %EdgeTimestampSaver{reporter: self()})
        |> child(:sink, %Sink{})

      Pipeline.execute_actions(pipeline, spec: [spec])

      assert_pipeline_notified(pipeline, :sink, :playing)

      for timestamp <- unquote(test_data.end_timestamps) do
        assert_receive {_genserver_atom, {:end_timestamp, :track_id, ^timestamp}}
      end

      if length(timestamps) != 0 do
        for idx <- 0..rtcp_events do
          rtcp = create_rtcp_report(1, 1, idx)
          assert_receive {_genserver_atom, {:rtcp, :track_id, ^rtcp}}
        end
      end

      assert_end_of_stream(pipeline, :sink)

      refute_received {_genserver_atom, {:rtcp, :track_id, _rtcp}}
      refute_received {_genserver_atom, {:end_timestamp, :track_id, _timestamp}}
    end
  end
end
