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

  describe "HLS Endpoint test" do
    test "creates correct hls stream from h264 file", %{rtc_engine: rtc_engine} do
      file_endpoint_id = "file-endpoint-id"

      file_name = "video.h264"
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
          nil,
          [:raw],
          nil,
          id: track_id
        )

      hls_endpoint = %HLS{
        rtc_engine: rtc_engine,
        owner: self(),
        output_directory: Path.join(["./", "test", "hls_output"]),
        target_window_duration: :infinity,
        framerate: {60, 1}
      }

      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, endpoint_id: hls_endpoint_id)

      parser = %Membrane.H264.FFmpeg.Parser{
        attach_nalus?: true,
        skip_until_parameters?: false,
        framerate: {60, 1}
      }

      file_endpoint = %FileEndpoint{
        rtc_engine: rtc_engine,
        file_path: file_path,
        track: track,
        interceptor: fn link_builder ->
          Membrane.ParentSpec.to(link_builder, :parser, parser)
        end
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
    end

    test "send cleanup notifiacation after removing pads", %{rtc_engine: rtc_engine} do
      video_file_endpoint_id = "file-endpoint-id"
      audio_file_endpoint_id = "test-file-endpoint-id"

      video_file_name = "one_second.h264"
      video_file_path = Path.join(@fixtures_dir, video_file_name)

      hls_endpoint_id = "hls-endpoint"

      video_track_id = "test-track-id"
      audio_track_id = "test-audio-track-id"
      stream_id = "test-stream-2"

      track =
        Engine.Track.new(
          :video,
          stream_id,
          video_file_endpoint_id,
          :H264,
          nil,
          [:raw],
          nil,
          id: video_track_id
        )

      hls_endpoint = %HLS{
        rtc_engine: rtc_engine,
        owner: self(),
        output_directory: Path.join(["./", "test", "hls_output"]),
        target_window_duration: :infinity,
        framerate: {60, 1}
      }

      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, endpoint_id: hls_endpoint_id)

      parser = %Membrane.H264.FFmpeg.Parser{
        attach_nalus?: true,
        skip_until_parameters?: false,
        framerate: {60, 1}
      }

      file_endpoint = %FileEndpoint{
        rtc_engine: rtc_engine,
        file_path: video_file_path,
        track: track,
        interceptor: fn link_builder ->
          Membrane.ParentSpec.to(link_builder, :parser, parser)
        end
      }

      audio_track =
        Engine.Track.new(
          :audio,
          stream_id,
          audio_file_endpoint_id,
          :AAC,
          nil,
          [:raw],
          nil,
          id: audio_track_id
        )

      test_file_endpoint = %FileEndpoint{
        rtc_engine: rtc_engine,
        file_path: Path.join(@fixtures_dir, "audio.aac"),
        track: audio_track,
        interceptor: fn link_builder ->
          Membrane.ParentSpec.to(link_builder, :parser, %Membrane.AAC.Parser{
            out_encapsulation: :none
          })
        end
      }

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint, endpoint_id: video_file_endpoint_id)

      assert_receive %Message.MediaEvent{rtc_engine: ^rtc_engine, to: :broadcast, data: data}

      assert %{
               "type" => "tracksAdded",
               "data" => %{
                 "trackIdToMetadata" => %{video_track_id => nil},
                 "peerId" => video_file_endpoint_id
               }
             } == Jason.decode!(data)

      :ok =
        Engine.add_endpoint(rtc_engine, test_file_endpoint, endpoint_id: audio_file_endpoint_id)

      assert_receive %Message.MediaEvent{rtc_engine: ^rtc_engine, to: :broadcast, data: data}

      assert %{
               "type" => "tracksAdded",
               "data" => %{
                 "trackIdToMetadata" => %{audio_track_id => nil},
                 "peerId" => audio_file_endpoint_id
               }
             } == Jason.decode!(data)

      Engine.message_endpoint(rtc_engine, video_file_endpoint_id, :start)
      Engine.message_endpoint(rtc_engine, audio_file_endpoint_id, :start)

      assert_receive({:playlist_playable, :video, ^stream_id, ^video_file_endpoint_id}, 5_000)
      assert_receive({:playlist_playable, :audio, ^stream_id, ^audio_file_endpoint_id}, 5_000)

      Process.sleep(10_000)

      output_dir = Path.join([@output_dir, stream_id])
      reference_dir = Path.join([@reference_dir, stream_id])

      directory_files = File.ls!(output_dir)
      assert Enum.sort(directory_files) == reference_dir |> File.ls!() |> Enum.sort()

      for file <- directory_files do
        output_path = Path.join(output_dir, file)
        reference_path = Path.join(reference_dir, file)

        assert File.read!(output_path) == File.read!(reference_path)
      end

      Engine.remove_endpoint(rtc_engine, video_file_endpoint_id)
      Engine.remove_endpoint(rtc_engine, audio_file_endpoint_id)

      assert_receive({:cleanup, _cleanup_function, ^stream_id})
    end
  end
end
