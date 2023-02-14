defmodule Membrane.RTC.HLSEndpointTest do
  use ExUnit.Case

  alias Membrane.HTTPAdaptiveStream.Sink.SegmentDuration
  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.HLS
  alias Membrane.RTC.Engine.Message
  alias Membrane.RTC.Engine.Support.FileEndpoint
  alias Membrane.RTC.Engine.Support.TestLayoutMaker

  alias Membrane.RTC.Engine.Endpoint.HLS.{
    AudioMixerConfig,
    CompositorConfig,
    MixerConfig,
    SinkBinConfig
  }

  @fixtures_dir "./test/fixtures/"
  @reference_dir "./test/hls_reference/"

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

  describe "HLS Endpoint test" do
    @describetag :tmp_dir

    test "creates correct hls stream from single (h264) input", %{
      rtc_engine: rtc_engine,
      tmp_dir: tmp_dir
    } do
      file_endpoint_id = "file-endpoint-id"

      file_name = "video.h264"
      file_path = Path.join(@fixtures_dir, file_name)

      hls_endpoint_id = "hls-endpoint"

      track_id = "test-track-id"
      stream_id = "test-stream"
      reference_id = "single-track-h264"

      hls_endpoint = create_hls_endpoint(rtc_engine, tmp_dir, false)
      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, endpoint_id: hls_endpoint_id)

      file_endpoint =
        create_video_file_endpoint(rtc_engine, file_path, stream_id, file_endpoint_id, track_id)

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint, endpoint_id: file_endpoint_id)

      assert_receive %Message.EndpointMessage{
        endpoint_id: ^file_endpoint_id,
        message: :tracks_added
      }

      Engine.message_endpoint(rtc_engine, file_endpoint_id, :start)

      assert_receive({:playlist_playable, :video, ^stream_id}, 10_000)

      Process.sleep(15_000)

      Engine.remove_endpoint(rtc_engine, file_endpoint_id)

      output_dir = Path.join([tmp_dir, stream_id])
      reference_dir = Path.join([@reference_dir, reference_id])

      directory_files = File.ls!(output_dir)

      assert Enum.sort(directory_files) == reference_dir |> File.ls!() |> Enum.sort()

      for file <- directory_files do
        output_path = Path.join(output_dir, file)
        reference_path = Path.join(reference_dir, file)

        assert File.read!(output_path) == File.read!(reference_path)
      end
    end

    test "creates correct hls stream from multiple (h264, opus) inputs belonging to the same stream",
         %{rtc_engine: rtc_engine, tmp_dir: tmp_dir} do
      video_file_endpoint_id = "video-file-endpoint"
      audio_file_endpoint_id = "audio-file-endpoint"

      video_file_name = "video.h264"
      video_file_path = Path.join(@fixtures_dir, video_file_name)

      hls_endpoint_id = "hls-endpoint"

      video_track_id = "test-video-track"
      audio_track_id = "test-audio-track"
      stream_id = "test-stream"
      reference_id = "multiple-tracks-h264-aac"

      hls_endpoint = create_hls_endpoint(rtc_engine, tmp_dir, false)

      audio_file_endpoint =
        create_audio_file_endnpoint(rtc_engine, stream_id, audio_file_endpoint_id, audio_track_id)

      video_file_endpoint =
        create_video_file_endpoint(
          rtc_engine,
          video_file_path,
          stream_id,
          video_file_endpoint_id,
          video_track_id
        )

      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, endpoint_id: hls_endpoint_id)

      :ok =
        Engine.add_endpoint(rtc_engine, video_file_endpoint, endpoint_id: video_file_endpoint_id)

      assert_receive %Message.EndpointMessage{
        endpoint_id: ^video_file_endpoint_id,
        message: :tracks_added
      }

      :ok =
        Engine.add_endpoint(rtc_engine, audio_file_endpoint, endpoint_id: audio_file_endpoint_id)

      assert_receive %Message.EndpointMessage{
        endpoint_id: ^audio_file_endpoint_id,
        message: :tracks_added
      }

      Engine.message_endpoint(rtc_engine, video_file_endpoint_id, :start)
      Engine.message_endpoint(rtc_engine, audio_file_endpoint_id, :start)

      assert_receive({:playlist_playable, :video, ^stream_id}, 5_000)
      assert_receive({:playlist_playable, :audio, ^stream_id}, 5_000)

      Process.sleep(15_000)

      output_dir = Path.join([tmp_dir, stream_id])
      reference_dir = Path.join([@reference_dir, reference_id])

      output_files = output_dir |> File.ls!() |> Enum.sort()
      reference_files = reference_dir |> File.ls!() |> Enum.sort()

      assert output_files == reference_files

      check_if_files_are_not_empty(output_files, output_dir)

      Engine.remove_endpoint(rtc_engine, video_file_endpoint_id)
      Engine.remove_endpoint(rtc_engine, audio_file_endpoint_id)
    end

    test "creates correct hls stream from multiple (h264, opus) inputs belonging to the same stream, muxed segments plus mixer",
         %{rtc_engine: rtc_engine, tmp_dir: tmp_dir} do
      video_file_endpoint_id = "video-file-endpoint"
      audio_file_endpoint_id = "audio-file-endpoint"

      video_file_name = "video.h264"
      video_file_path = Path.join(@fixtures_dir, video_file_name)

      hls_endpoint_id = "hls-endpoint"

      video_track_id = "test-video-track"
      audio_track_id = "test-audio-track"
      stream_id = "test-stream"
      reference_id = "muxed-segments"

      hls_endpoint = create_hls_endpoint_with_muxed_segments(rtc_engine, tmp_dir, false)

      audio_file_endpoint =
        create_audio_file_endnpoint(rtc_engine, stream_id, audio_file_endpoint_id, audio_track_id)

      video_file_endpoint =
        create_video_file_endpoint(
          rtc_engine,
          video_file_path,
          stream_id,
          video_file_endpoint_id,
          video_track_id
        )

      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, endpoint_id: hls_endpoint_id)

      :ok =
        Engine.add_endpoint(rtc_engine, video_file_endpoint, endpoint_id: video_file_endpoint_id)

      assert_receive %Message.EndpointMessage{
        endpoint_id: ^video_file_endpoint_id,
        message: :tracks_added
      }

      :ok =
        Engine.add_endpoint(rtc_engine, audio_file_endpoint, endpoint_id: audio_file_endpoint_id)

      assert_receive %Message.EndpointMessage{
        endpoint_id: ^audio_file_endpoint_id,
        message: :tracks_added
      }

      Engine.message_endpoint(rtc_engine, video_file_endpoint_id, :start)
      Engine.message_endpoint(rtc_engine, audio_file_endpoint_id, :start)

      Process.sleep(15_000)

      assert_receive({:playlist_playable, :video, ""})

      Engine.remove_endpoint(rtc_engine, video_file_endpoint_id)
      Engine.remove_endpoint(rtc_engine, audio_file_endpoint_id)

      output_dir = tmp_dir
      reference_dir = Path.join([@reference_dir, reference_id])

      output_files = output_dir |> File.ls!() |> Enum.sort()
      reference_files = reference_dir |> File.ls!() |> Enum.sort()

      assert output_files == reference_files

      check_if_files_are_not_empty(output_files, output_dir)
    end

    test "video mixer works properly", %{
      rtc_engine: rtc_engine,
      tmp_dir: tmp_dir
    } do
      file_name = "video.h264"
      file_path = Path.join(@fixtures_dir, file_name)
      reference_id = "video_mixer"

      hls_endpoint_id = "hls-endpoint"

      track_id = "test-track-id"
      track_id_2 = "test-track-id_2"
      stream_id = "video-mixer-test-stream"
      stream_id_2 = "video-mixer-test-stream_2"
      file_endpoint_id = "video-mixer-file-endpoint-id"
      file_endpoint_id_2 = "video-mixer-file-endpoint-id_2"

      hls_endpoint = create_hls_endpoint_with_mixer(rtc_engine, tmp_dir, true)
      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, endpoint_id: hls_endpoint_id)

      file_endpoint =
        create_video_file_endpoint(rtc_engine, file_path, stream_id, file_endpoint_id, track_id)

      file_endpoint_2 =
        create_video_file_endpoint(
          rtc_engine,
          file_path,
          stream_id_2,
          file_endpoint_id_2,
          track_id_2
        )

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint, endpoint_id: file_endpoint_id)

      assert_receive %Message.EndpointMessage{
        endpoint_id: ^file_endpoint_id,
        message: :tracks_added
      }

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint_2, endpoint_id: file_endpoint_id_2)

      assert_receive %Message.EndpointMessage{
        endpoint_id: ^file_endpoint_id_2,
        message: :tracks_added
      }

      Engine.message_endpoint(rtc_engine, file_endpoint_id, :start)
      Engine.message_endpoint(rtc_engine, file_endpoint_id_2, :start)

      Process.sleep(20_000)

      assert_receive({:playlist_playable, :audio, _playlist_idl}, 10_000)
      assert_receive({:playlist_playable, :video, _playlist_idl}, 10_000)

      Engine.remove_endpoint(rtc_engine, file_endpoint_id)
      Engine.remove_endpoint(rtc_engine, file_endpoint_id_2)

      output_dir = tmp_dir
      reference_dir = Path.join([@reference_dir, reference_id])

      output_files = File.ls!(output_dir)

      assert Enum.sort(output_files) == reference_dir |> File.ls!() |> Enum.sort()

      check_if_files_are_not_empty(output_files, output_dir)

      # if number of header files is greater than 1, video mixer is not working properly
      assert Enum.count(output_files, &String.starts_with?(&1, "video_header")) == 1
    end

    test "audio mixer works properly", %{
      rtc_engine: rtc_engine,
      tmp_dir: tmp_dir
    } do
      reference_id = "audio_mixer"
      hls_endpoint_id = "hls-endpoint"

      track_id = "test-track-id"
      track_id_2 = "test-track-id_2"
      stream_id = "audio-mixer-test-stream"
      stream_id_2 = "audio-mixer-test-stream_2"
      file_endpoint_id = "audio-mixer-file-endpoint-id"
      file_endpoint_id_2 = "audio-mixer-file-endpoint-id_2"

      hls_endpoint = create_hls_endpoint_with_mixer(rtc_engine, tmp_dir, true)
      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, endpoint_id: hls_endpoint_id)

      file_endpoint =
        create_audio_file_endnpoint(rtc_engine, stream_id, file_endpoint_id, track_id)

      file_endpoint_2 =
        create_audio_file_endnpoint(rtc_engine, stream_id_2, file_endpoint_id_2, track_id_2)

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint, endpoint_id: file_endpoint_id)

      assert_receive %Message.EndpointMessage{
        endpoint_id: ^file_endpoint_id,
        message: :tracks_added
      }

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint_2, endpoint_id: file_endpoint_id_2)

      assert_receive %Message.EndpointMessage{
        endpoint_id: ^file_endpoint_id_2,
        message: :tracks_added
      }

      Engine.message_endpoint(rtc_engine, file_endpoint_id, :start)
      Engine.message_endpoint(rtc_engine, file_endpoint_id_2, :start)

      Process.sleep(15_000)

      assert_receive({:playlist_playable, :audio, _playlist_idl}, 10_000)
      assert_receive({:playlist_playable, :video, _playlist_idl}, 10_000)

      Engine.remove_endpoint(rtc_engine, file_endpoint_id)
      Engine.remove_endpoint(rtc_engine, file_endpoint_id_2)

      output_dir = tmp_dir
      reference_dir = Path.join([@reference_dir, reference_id])

      output_files = File.ls!(output_dir)

      assert Enum.sort(output_files) == reference_dir |> File.ls!() |> Enum.sort()

      check_if_files_are_not_empty(output_files, output_dir)
    end
  end

  test "handling update_layout message works correctly" do
    state = %{
      video_layout_tracks_added: %{},
      video_layout_state: TestLayoutMaker.init(nil),
      tracks: %{
        first: %{id: :first},
        second: %{id: :second}
      },
      mixer_config: %{video: %{layout_module: TestLayoutMaker}}
    }

    state = send_update_layout_notification(:first, state)
    assert {:track_added, :first} == state.video_layout_state.last_callback_invoked
    state = send_update_layout_notification(:first, state)
    assert {:track_updated, :first} == state.video_layout_state.last_callback_invoked
    state = send_update_layout_notification(:second, state)
    assert {:track_added, :second} == state.video_layout_state.last_callback_invoked
    state = send_update_layout_notification(:second, state)
    assert {:track_updated, :second} == state.video_layout_state.last_callback_invoked
  end

  defp send_update_layout_notification(track_id, state) do
    stream_format = %Membrane.RawVideo{
      width: 400,
      height: 800,
      pixel_format: :I420,
      framerate: {24, 1},
      aligned: true
    }

    {_actions, state} =
      HLS.handle_child_notification(
        {:update_layout, stream_format},
        {:stream_format_updater, track_id},
        nil,
        state
      )

    state
  end

  defp create_hls_endpoint(rtc_engine, output_dir, _transcoding?) do
    %HLS{
      rtc_engine: rtc_engine,
      owner: self(),
      output_directory: output_dir,
      sink_bin_config: %SinkBinConfig{mode: :vod, target_window_duration: :infinity}
    }
  end

  defp create_hls_endpoint_with_mixer(rtc_engine, output_dir, _transcoding?) do
    mixer_config = %MixerConfig{
      video: %CompositorConfig{
        background: %Membrane.BlankVideoGenerator{
          stream_format: %CompositorConfig{}.stream_format,
          duration: Membrane.Time.seconds(10)
        }
      },
      audio: %AudioMixerConfig{
        background: %Membrane.SilenceGenerator{
          stream_format: %AudioMixerConfig{}.stream_format,
          duration: Membrane.Time.seconds(10),
          frames_per_buffer: 960
        }
      }
    }

    %HLS{
      rtc_engine: rtc_engine,
      owner: self(),
      output_directory: output_dir,
      mixer_config: mixer_config,
      segment_duration: SegmentDuration.new(Membrane.Time.seconds(2), Membrane.Time.seconds(3)),
      sink_bin_config: %SinkBinConfig{mode: :vod, target_window_duration: :infinity}
    }
  end

  defp create_hls_endpoint_with_muxed_segments(rtc_engine, output_dir, _transcoding?) do
    mixer_config = %MixerConfig{
      video: %CompositorConfig{
        background: %Membrane.BlankVideoGenerator{
          stream_format: %CompositorConfig{}.stream_format,
          duration: Membrane.Time.seconds(10)
        }
      },
      audio: %AudioMixerConfig{
        background: %Membrane.SilenceGenerator{
          stream_format: %AudioMixerConfig{}.stream_format,
          duration: Membrane.Time.seconds(10),
          frames_per_buffer: 960
        }
      }
    }

    %HLS{
      rtc_engine: rtc_engine,
      owner: self(),
      output_directory: output_dir,
      mixer_config: mixer_config,
      segment_duration: SegmentDuration.new(Membrane.Time.seconds(2), Membrane.Time.seconds(3)),
      sink_bin_config: %SinkBinConfig{
        hls_mode: :muxed_av,
        mode: :vod,
        target_window_duration: :infinity
      }
    }
  end

  defp create_video_file_endpoint(
         rtc_engine,
         video_file_path,
         stream_id,
         video_file_endpoint_id,
         video_track_id
       ) do
    video_track =
      Engine.Track.new(
        :video,
        stream_id,
        video_file_endpoint_id,
        :H264,
        90_000,
        nil,
        id: video_track_id,
        metadata: %{"mainPresenter" => true}
      )

    %FileEndpoint{
      rtc_engine: rtc_engine,
      file_path: video_file_path,
      track: video_track,
      ssrc: 1234,
      payload_type: 96
    }
  end

  defp create_audio_file_endnpoint(rtc_engine, stream_id, audio_file_endpoint_id, audio_track_id) do
    audio_track =
      Engine.Track.new(
        :audio,
        stream_id,
        audio_file_endpoint_id,
        :OPUS,
        48_000,
        nil,
        id: audio_track_id
      )

    %FileEndpoint{
      rtc_engine: rtc_engine,
      file_path: Path.join(@fixtures_dir, "audio.aac"),
      track: audio_track,
      ssrc: 2345,
      payload_type: 108
    }
  end

  defp check_if_files_are_not_empty(output_files, output_dir) do
    for output_file <- output_files do
      output_path = Path.join(output_dir, output_file)
      %{size: size} = File.stat!(output_path)
      assert size > 0
    end
  end
end
