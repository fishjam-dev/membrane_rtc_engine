defmodule Membrane.RTC.FileEndpointTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint
  alias Membrane.RTC.Engine.Message
  alias Membrane.RTC.Engine.Support

  @fixtures_dir "./test/fixtures/"

  setup do
    options = [
      id: "test_rtc"
    ]

    {:ok, pid} = Engine.start_link(options, [])

    Engine.register(pid, self())

    on_exit(fn ->
      Engine.terminate(pid, blocking?: true)
    end)

    [rtc_engine: pid]
  end

  @source_endpoint_id "source-endpoint-id"
  @sink_endpoint_id "sink-endpoint-id"
  @track_id "test-track-id"
  @stream_id "test-stream"

  describe "File Endpoint test" do
    @tag :tmp_dir
    test "OPUS test", %{
      rtc_engine: rtc_engine,
      tmp_dir: tmp_dir
    } do
      file_name = "audio.opus"
      file_path = Path.join(@fixtures_dir, file_name)

      output_file = Path.join([tmp_dir, "#{@track_id}.opus"])

      :ok =
        Engine.add_endpoint(
          rtc_engine,
          %Support.Sink{
            rtc_engine: rtc_engine,
            file_path: output_file
          },
          id: @sink_endpoint_id
        )

      file_endpoint =
        create_audio_file_endpoint(
          rtc_engine,
          file_path,
          @stream_id,
          @source_endpoint_id,
          @track_id
        )

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint, id: @source_endpoint_id)

      assert_receive %Message.EndpointMessage{
        endpoint_id: @source_endpoint_id,
        message: :tracks_added
      }

      assert_receive %Message.EndpointMessage{
        message: :tracks_subscribed
      }

      Endpoint.File.start_sending(rtc_engine, @source_endpoint_id)

      refute_receive %Message.EndpointCrashed{endpoint_id: @sink_endpoint_id}

      assert_receive %Message.EndpointMessage{
                       message: :finished,
                       endpoint_id: @source_endpoint_id
                     },
                     25_000

      assert_receive %Message.EndpointMessage{message: :finished, endpoint_id: @sink_endpoint_id},
                     25_000

      :ok = Engine.remove_endpoint(rtc_engine, @sink_endpoint_id)

      assert File.exists?(output_file)
      assert File.read!(output_file) |> byte_size() == File.read!(file_path) |> byte_size()
      assert File.read!(output_file) == File.read!(file_path)
    end

    @tag :tmp_dir
    test "h264 test", %{
      rtc_engine: rtc_engine,
      tmp_dir: tmp_dir
    } do
      file_name = "video.h264"
      file_path = Path.join(@fixtures_dir, file_name)

      output_file = Path.join([tmp_dir, "#{@track_id}.h264"])

      :ok =
        Engine.add_endpoint(
          rtc_engine,
          %Support.Sink{
            rtc_engine: rtc_engine,
            file_path: output_file
          },
          id: @sink_endpoint_id
        )

      file_endpoint =
        create_video_file_endpoint(
          rtc_engine,
          file_path,
          @stream_id,
          @source_endpoint_id,
          @track_id
        )

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint, id: @source_endpoint_id)

      assert_receive %Message.EndpointMessage{
        endpoint_id: @source_endpoint_id,
        message: :tracks_added
      }

      assert_receive %Message.EndpointMessage{
        message: :tracks_subscribed
      }

      Endpoint.File.start_sending(rtc_engine, @source_endpoint_id)

      refute_receive %Message.EndpointCrashed{endpoint_id: @sink_endpoint_id}

      assert_receive %Message.EndpointMessage{
                       message: :finished,
                       endpoint_id: @source_endpoint_id
                     },
                     25_000

      assert_receive %Message.EndpointMessage{message: :finished, endpoint_id: @sink_endpoint_id},
                     25_000

      :ok = Engine.remove_endpoint(rtc_engine, @sink_endpoint_id)

      assert File.exists?(output_file)
      assert File.read!(output_file) |> byte_size() == File.read!(file_path) |> byte_size()
      assert File.read!(output_file) == File.read!(file_path)
    end
  end

  defp create_video_file_endpoint(
         rtc_engine,
         video_file_path,
         stream_id,
         video_source_endpoint_id,
         video_track_id
       ) do
    video_track =
      Engine.Track.new(
        :video,
        stream_id,
        video_source_endpoint_id,
        :H264,
        90_000,
        %ExSDP.Attribute.FMTP{
          pt: 96
        },
        id: video_track_id,
        framerate: {60, 1}
      )

    %Endpoint.File{
      rtc_engine: rtc_engine,
      file_path: video_file_path,
      track: video_track,
      ssrc: 1234,
      payload_type: 96
    }
  end

  defp create_audio_file_endpoint(
         rtc_engine,
         audio_file_path,
         stream_id,
         audio_source_endpoint_id,
         audio_track_id
       ) do
    ext = String.split(audio_file_path, ".") |> List.last()

    encoding =
      case ext do
        "aac" -> :AAC
        "opus" -> :OPUS
      end

    audio_track =
      Engine.Track.new(
        :audio,
        stream_id,
        audio_source_endpoint_id,
        encoding,
        48_000,
        nil,
        id: audio_track_id
      )

    %Endpoint.File{
      rtc_engine: rtc_engine,
      file_path: audio_file_path,
      track: audio_track,
      ssrc: 2345,
      payload_type: 108
    }
  end
end
