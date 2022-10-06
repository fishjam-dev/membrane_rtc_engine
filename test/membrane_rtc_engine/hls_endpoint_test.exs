defmodule Membrane.RTC.HLSEndpointTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.HLS
  alias Membrane.RTC.Engine.{Message}
  alias Membrane.RTC.Engine.Support.FileEndpoint

  @fixtures_dir "./test/fixtures/"
  @reference_dir "./test/hls_reference/"
  @output_dir "./test/hls_output/"

  setup do
    options = [
      id: "test_rtc"
    ]

    {:ok, pid} = Engine.start_link(options, [])

    Engine.register(pid, self())

    [rtc_engine: pid]
  end

  describe "HLS Endpoint" do
    test "creates correct hls stream from h264 file", %{rtc_engine: rtc_engine} do
      # FIXME MV-147
      # after engine refactor this test
      # produces video with glitches

      file_endpoint_id = "file-endpoint-id"

      file_name = "rtp_h264_video"
      file_path = Path.join(@fixtures_dir, file_name)

      hls_endpoint_id = "hls-endpoint"

      track_id = "test-track-id"
      stream_id = "test-stream"

      track =
        Engine.Track.new(
          :video,
          stream_id,
          file_endpoint_id,
          :H264,
          90_000,
          nil,
          id: track_id
        )

      hls_endpoint = %HLS{
        rtc_engine: rtc_engine,
        owner: self(),
        output_directory: Path.join(["./", "test", "hls_output"]),
        target_window_duration: :infinity,
        target_segment_duration: Membrane.Time.seconds(10)
      }

      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, endpoint_id: hls_endpoint_id)

      file_endpoint = %FileEndpoint{
        rtc_engine: rtc_engine,
        file_path: file_path,
        track: track
      }

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint, endpoint_id: file_endpoint_id)

      assert_receive %Message.MediaEvent{rtc_engine: ^rtc_engine, to: :broadcast, data: data}

      assert %{
               "type" => "tracksAdded",
               "data" => %{
                 "trackIdToMetadata" => %{track_id => nil},
                 "peerId" => file_endpoint_id
               }
             } == Jason.decode!(data)

      Engine.message_endpoint(rtc_engine, file_endpoint_id, :start)

      assert_receive({:playlist_playable, :video, ^stream_id, ^file_endpoint_id}, 5_000)

      Process.sleep(15_000)

      Engine.remove_endpoint(rtc_engine, file_endpoint_id)

      output_dir = Path.join([@output_dir, stream_id])
      reference_dir = Path.join([@reference_dir, stream_id])

      directory_files = File.ls!(output_dir)

      assert Enum.sort(directory_files) == reference_dir |> File.ls!() |> Enum.sort()

      for file <- directory_files do
        output_path = Path.join(output_dir, file)
        reference_path = Path.join(reference_dir, file)

        assert File.read!(output_path) == File.read!(reference_path)
      end

      assert_receive {:cleanup, _cleanup_function, ^stream_id}

      File.rm_rf!(@output_dir)

      Engine.terminate(rtc_engine, blocking?: true)
    end
  end
end
