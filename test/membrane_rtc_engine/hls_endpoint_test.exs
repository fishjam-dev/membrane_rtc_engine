defmodule Membrane.RTC.HLSEndpointTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.HLS
  alias Membrane.RTC.Engine.Message
  alias Membrane.RTC.Engine.Support.FileEndpoint
  alias Membrane.RTC.Engine.Support.TestLayoutMaker

  alias Membrane.RTC.Engine.Endpoint.HLS.{
    AudioMixerConfig,
    CompositorConfig,
    HLSConfig,
    MixerConfig
  }

  @fixtures_dir "./test/fixtures/"
  @reference_dir "./test/hls_reference/"
  @main_manifest "index.m3u8"
  @acceptable_bandwidth_diff 500_000

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

    # FIXME: Test prints logs informing of parent crashing (HLS Endpoint)
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

      output_dir = Path.join([tmp_dir, stream_id])
      reference_dir = Path.join([@reference_dir, reference_id])

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

      assert_receive({:playlist_playable, :video, ^output_dir}, 10_000)

      assert_receive %Membrane.RTC.Engine.Message.EndpointMessage{
                       endpoint_id: ^hls_endpoint_id,
                       message: {:end_of_stream, ^stream_id}
                     },
                     25_000

      check_correctness_of_output_files(output_dir, reference_dir)
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

      output_dir = Path.join([tmp_dir, stream_id])
      reference_dir = Path.join([@reference_dir, reference_id])

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

      assert_receive({:playlist_playable, :video, ^output_dir}, 10_000)
      assert_receive({:playlist_playable, :audio, ^output_dir}, 10_000)

      assert_receive %Membrane.RTC.Engine.Message.EndpointMessage{
                       endpoint_id: ^hls_endpoint_id,
                       message: {:end_of_stream, ^stream_id}
                     },
                     25_000

      check_correctness_of_output_files(output_dir, reference_dir)
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

      output_dir = tmp_dir
      reference_dir = Path.join([@reference_dir, reference_id])

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

      assert_receive({:playlist_playable, :video, ^output_dir}, 50_000)

      assert_receive %Membrane.RTC.Engine.Message.EndpointMessage{
                       endpoint_id: ^hls_endpoint_id,
                       message: {:end_of_stream, :muxed}
                     },
                     25_000

      check_correctness_of_output_files(output_dir, reference_dir)
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

      output_dir = tmp_dir
      reference_dir = Path.join([@reference_dir, reference_id])

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

      assert_receive({:playlist_playable, :audio, ^output_dir}, 50_000)
      assert_receive({:playlist_playable, :video, ^output_dir}, 50_000)

      assert_receive %Membrane.RTC.Engine.Message.EndpointMessage{
                       endpoint_id: ^hls_endpoint_id,
                       message: {:end_of_stream, :muxed}
                     },
                     25_000

      check_correctness_of_output_files(output_dir, reference_dir)

      output_files = output_dir |> File.ls!() |> Enum.sort()
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

      output_dir = tmp_dir
      reference_dir = Path.join([@reference_dir, reference_id])

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

      assert_receive({:playlist_playable, :audio, ^output_dir}, 50_000)
      assert_receive({:playlist_playable, :video, ^output_dir}, 50_000)

      assert_receive %Membrane.RTC.Engine.Message.EndpointMessage{
                       endpoint_id: ^hls_endpoint_id,
                       message: {:end_of_stream, :muxed}
                     },
                     25_000

      check_correctness_of_output_files(output_dir, reference_dir)
    end
  end

  test "handling update_layout message works correctly" do
    state = %{
      video_layout_tracks_added: %{},
      video_layout_state: TestLayoutMaker.init(nil, nil),
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
      synchronize_tracks?: false,
      hls_config: %HLSConfig{mode: :vod, target_window_duration: :infinity, segment_duration: Membrane.Time.seconds(4)}
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
      synchronize_tracks?: false,
      mixer_config: mixer_config,
      hls_config: %HLSConfig{
        mode: :vod,
        target_window_duration: :infinity,
        segment_duration: Membrane.Time.seconds(4)
      }
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
      synchronize_tracks?: false,
      mixer_config: mixer_config,
      hls_config: %HLSConfig{
        hls_mode: :muxed_av,
        mode: :vod,
        target_window_duration: :infinity,
        segment_duration: Membrane.Time.seconds(3)
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
        %ExSDP.Attribute.FMTP{
          pt: 96
        },
        id: video_track_id,
        metadata: %{"mainPresenter" => true, "isScreenSharing" => false}
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

  defp check_correctness_of_output_files(output_dir, reference_dir) do
    output_files = output_dir |> File.ls!() |> Enum.sort()

    # output_manifests =
    #   Enum.filter(output_files, fn file ->
    #     String.match?(file, ~r/\.m3u8$/) and file != @main_manifest
    #   end)

    # FIXME:
    # for manifest <- output_manifests do
    #   manifest_path = Path.join(output_dir, manifest)
    #   reference_path = Path.join(reference_dir, manifest)

    #   assert File.read!(manifest_path) == File.read!(reference_path)
    # end

    compare_main_manifests(output_dir, reference_dir)

    reference_files = reference_dir |> File.ls!() |> Enum.sort()
    assert output_files == reference_files

    for output_file <- output_files do
      output_path = Path.join(output_dir, output_file)
      %{size: size} = File.stat!(output_path)
      assert size > 0
    end
  end

  defp compare_main_manifests(output_dir, reference_dir) do
    output_file = Path.join(output_dir, @main_manifest) |> File.read!()
    reference_file = Path.join(reference_dir, @main_manifest) |> File.read!()

    %{
      bandwidth: ["#EXT-X-STREAM-INF:BANDWIDTH=" <> output_bandwidth],
      other: output_without_bandwidth
    } = filter_manifest(output_file)

    %{
      bandwidth: ["#EXT-X-STREAM-INF:BANDWIDTH=" <> reference_bandwidth],
      other: reference_without_bandwidth
    } = filter_manifest(reference_file)

    output_bandwidth = String.to_integer(output_bandwidth)
    reference_bandwidth = String.to_integer(reference_bandwidth)

    assert output_bandwidth + @acceptable_bandwidth_diff > reference_bandwidth and
             output_bandwidth < reference_bandwidth + @acceptable_bandwidth_diff

    assert output_without_bandwidth == reference_without_bandwidth
  end

  defp filter_manifest(index_file) do
    index_file
    |> String.split(["\n", ","])
    |> Enum.group_by(fn x ->
      if String.starts_with?(x, "#EXT-X-STREAM-INF:"), do: :bandwidth, else: :other
    end)
  end
end
