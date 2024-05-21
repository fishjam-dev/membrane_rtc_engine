defmodule Membrane.RTC.Engine.Endpoint.Recording.EdgeTimestampSaverTest do
  use ExUnit.Case, async: true

  import Membrane.Testing.Assertions
  import Membrane.ChildrenSpec

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Endpoint.Recording.EdgeTimestampSaver
  alias Membrane.Testing.{Pipeline, Sink, Source}

  tests_data = [
    %{
      title: "happy path",
      timestamps: Enum.to_list(1..200),
      end_timestamps: [101, 200]
    },
    %{
      title: "edge timestamp not in order",
      timestamps: Enum.to_list(1..98) ++ [100, 99] ++ Enum.to_list(101..200),
      end_timestamps: [101, 200]
    },
    %{
      title: "Long sequence not in order",
      timestamps: Enum.to_list(1..50) ++ Enum.to_list(100..50) ++ Enum.to_list(101..150),
      end_timestamps: [101, 150]
    },
    %{
      title: "End is not in order",
      timestamps: Enum.to_list(1..50) ++ Enum.to_list(70..50),
      end_timestamps: [70]
    }
  ]

  for test_data <- tests_data do
    test test_data.title do
      pipeline = Pipeline.start_link_supervised!()

      buffers =
        for i <- unquote(test_data.timestamps) do
          %Buffer{payload: <<>>, metadata: %{rtp: %{timestamp: i}}}
        end

      spec =
        child(:source, %Source{output: buffers})
        |> child({:edge_timestamp_saver, :track_id}, %EdgeTimestampSaver{reporter: self()})
        |> child(:sink, %Sink{})

      Pipeline.execute_actions(pipeline, spec: [spec])

      assert_pipeline_notified(pipeline, :sink, :playing)

      for timestamp <- unquote(test_data.end_timestamps) do
        assert_receive {_genserver_atom, {:end_timestamp, :track_id, ^timestamp}}
      end

      assert_end_of_stream(pipeline, :sink)

      refute_received {_genserver_atom, {:end_timestamp, :track_id, _timestamp}}
    end
  end
end
