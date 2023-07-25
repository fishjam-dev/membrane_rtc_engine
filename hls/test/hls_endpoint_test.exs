defmodule Membrane.RTC.HLSEndpointTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.HLS
  alias Membrane.RTC.Engine.Message
  alias Membrane.RTC.Engine.Testing.FileSourceEndpoint
  alias Membrane.RTC.Engine.Endpoint.HLS.{HLSConfig, MixerConfig}

  @fixtures_dir "./test/fixtures/"
  @playlist_playable_delay 20_000
  @segment_delay 25_000
  @tracks_added_delay 500

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

      output_dir = Path.join([tmp_dir, stream_id])

      hls_endpoint = create_hls_endpoint(rtc_engine, tmp_dir, :single)
      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, id: hls_endpoint_id)

      file_endpoint =
        create_video_file_endpoint(rtc_engine, file_path, stream_id, file_endpoint_id, track_id)

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint, id: file_endpoint_id)

      assert_receive %Message.EndpointMessage{
                       endpoint_id: ^file_endpoint_id,
                       message: :tracks_added
                     },
                     @tracks_added_delay

      Engine.message_endpoint(rtc_engine, file_endpoint_id, :start)

      assert_receive({:playlist_playable, :video, ^output_dir}, @playlist_playable_delay)
      assert_receive({:segment, "video_segment_1" <> _}, @segment_delay)
      assert_receive({:manifest, %{video_segments: 2}})

      Engine.remove_endpoint(rtc_engine, hls_endpoint_id)

      check_separate_hls_playlist(output_dir, 2, 2)
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

      output_dir = Path.join([tmp_dir, stream_id])

      hls_endpoint = create_hls_endpoint(rtc_engine, tmp_dir, :multiple)

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

      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, id: hls_endpoint_id)
      :ok = Engine.add_endpoint(rtc_engine, video_file_endpoint, id: video_file_endpoint_id)

      assert_receive %Message.EndpointMessage{
                       endpoint_id: ^video_file_endpoint_id,
                       message: :tracks_added
                     },
                     @tracks_added_delay

      :ok = Engine.add_endpoint(rtc_engine, audio_file_endpoint, id: audio_file_endpoint_id)

      assert_receive %Message.EndpointMessage{
                       endpoint_id: ^audio_file_endpoint_id,
                       message: :tracks_added
                     },
                     @tracks_added_delay

      Engine.message_endpoint(rtc_engine, video_file_endpoint_id, :start)
      Engine.message_endpoint(rtc_engine, audio_file_endpoint_id, :start)

      assert_receive({:playlist_playable, :video, ^output_dir}, @playlist_playable_delay)
      assert_receive({:playlist_playable, :audio, ^output_dir}, @playlist_playable_delay)

      assert_receive({:segment, "video_segment_1" <> _}, @segment_delay)
      assert_receive({:segment, "audio_segment_1" <> _}, @segment_delay)
      assert_receive({:manifest, %{audio_segments: 2}})
      assert_receive({:manifest, %{video_segments: 2}})

      Engine.remove_endpoint(rtc_engine, hls_endpoint_id)

      check_separate_hls_playlist(output_dir, 2, 3)
    end

    @tag :gpu
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

      hls_endpoint = create_hls_endpoint(rtc_engine, tmp_dir, :muxed, %MixerConfig{})

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

      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, id: hls_endpoint_id)
      :ok = Engine.add_endpoint(rtc_engine, video_file_endpoint, id: video_file_endpoint_id)

      assert_receive %Message.EndpointMessage{
                       endpoint_id: ^video_file_endpoint_id,
                       message: :tracks_added
                     },
                     @tracks_added_delay

      :ok = Engine.add_endpoint(rtc_engine, audio_file_endpoint, id: audio_file_endpoint_id)

      assert_receive %Message.EndpointMessage{
                       endpoint_id: ^audio_file_endpoint_id,
                       message: :tracks_added
                     },
                     @tracks_added_delay

      Engine.message_endpoint(rtc_engine, video_file_endpoint_id, :start)
      Engine.message_endpoint(rtc_engine, audio_file_endpoint_id, :start)

      assert_receive({:playlist_playable, :video, ^tmp_dir}, @playlist_playable_delay)
      assert_receive({:segment, "muxed_segment_1" <> _}, @segment_delay)
      assert_receive({:manifest, %{muxed_segments: 2}})

      Engine.remove_endpoint(rtc_engine, hls_endpoint_id)

      check_muxed_hls_playlist(tmp_dir, 2, 2)
    end

    @tag :gpu
    test "video mixer works properly", %{
      rtc_engine: rtc_engine,
      tmp_dir: tmp_dir
    } do
      file_name = "video.h264"
      file_path = Path.join(@fixtures_dir, file_name)

      hls_endpoint_id = "hls-endpoint"

      track_id = "test-track-id"
      track_id_2 = "test-track-id_2"
      stream_id = "video-mixer-test-stream"
      stream_id_2 = "video-mixer-test-stream_2"
      file_endpoint_id = "video-mixer-file-endpoint-id"
      file_endpoint_id_2 = "video-mixer-file-endpoint-id_2"

      hls_endpoint = create_hls_endpoint(rtc_engine, tmp_dir, :multiple, %MixerConfig{})
      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, id: hls_endpoint_id)

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

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint, id: file_endpoint_id)

      assert_receive %Message.EndpointMessage{
                       endpoint_id: ^file_endpoint_id,
                       message: :tracks_added
                     },
                     @tracks_added_delay

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint_2, id: file_endpoint_id_2)

      assert_receive %Message.EndpointMessage{
                       endpoint_id: ^file_endpoint_id_2,
                       message: :tracks_added
                     },
                     @tracks_added_delay

      Engine.message_endpoint(rtc_engine, file_endpoint_id, :start)
      Engine.message_endpoint(rtc_engine, file_endpoint_id_2, :start)

      assert_receive({:playlist_playable, :audio, ^tmp_dir}, @playlist_playable_delay)
      assert_receive({:playlist_playable, :video, ^tmp_dir}, @playlist_playable_delay)

      assert_receive({:segment, "video_segment_1" <> _}, @segment_delay)
      assert_receive({:segment, "audio_segment_1" <> _}, @segment_delay)
      assert_receive({:manifest, %{audio_segments: 2}})
      assert_receive({:manifest, %{video_segments: 2}})

      Engine.remove_endpoint(rtc_engine, hls_endpoint_id)

      check_separate_hls_playlist(tmp_dir, 2, 3)
    end

    @tag :gpu
    test "audio mixer works properly", %{
      rtc_engine: rtc_engine,
      tmp_dir: tmp_dir
    } do
      hls_endpoint_id = "hls-endpoint"

      track_id = "test-track-id"
      track_id_2 = "test-track-id_2"
      stream_id = "audio-mixer-test-stream"
      stream_id_2 = "audio-mixer-test-stream_2"
      file_endpoint_id = "audio-mixer-file-endpoint-id"
      file_endpoint_id_2 = "audio-mixer-file-endpoint-id_2"

      hls_endpoint = create_hls_endpoint(rtc_engine, tmp_dir, :multiple, %MixerConfig{})
      :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, id: hls_endpoint_id)

      file_endpoint =
        create_audio_file_endnpoint(rtc_engine, stream_id, file_endpoint_id, track_id)

      file_endpoint_2 =
        create_audio_file_endnpoint(rtc_engine, stream_id_2, file_endpoint_id_2, track_id_2)

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint, id: file_endpoint_id)

      assert_receive %Message.EndpointMessage{
                       endpoint_id: ^file_endpoint_id,
                       message: :tracks_added
                     },
                     @tracks_added_delay

      :ok = Engine.add_endpoint(rtc_engine, file_endpoint_2, id: file_endpoint_id_2)

      assert_receive %Message.EndpointMessage{
                       endpoint_id: ^file_endpoint_id_2,
                       message: :tracks_added
                     },
                     @tracks_added_delay

      Engine.message_endpoint(rtc_engine, file_endpoint_id, :start)
      Engine.message_endpoint(rtc_engine, file_endpoint_id_2, :start)

      assert_receive({:playlist_playable, :audio, ^tmp_dir}, @playlist_playable_delay)
      assert_receive({:playlist_playable, :video, ^tmp_dir}, @playlist_playable_delay)

      assert_receive({:segment, "video_segment_1" <> _}, @segment_delay)
      assert_receive({:segment, "audio_segment_1" <> _}, @segment_delay)
      assert_receive({:manifest, %{audio_segments: 2}})
      assert_receive({:manifest, %{video_segments: 2}})

      Engine.remove_endpoint(rtc_engine, hls_endpoint_id)

      check_separate_hls_playlist(tmp_dir, 2, 3)
    end
  end

  defp create_hls_endpoint(rtc_engine, output_dir, input_type, mixer_config \\ nil) do
    pid = self()

    should_stop? =
      case input_type do
        :single ->
          fn
            %{video_segments: video} ->
              video > 1
          end

        :muxed ->
          fn
            %{muxed_segments: muxed} ->
              muxed > 1
          end

        :multiple ->
          fn
            %{video_segments: video, audio_segments: audio} ->
              video > 1 and audio > 1
          end
      end

    storage = fn directory ->
      %Membrane.RTC.Engine.Support.HLSStorage{
        pid: pid,
        file_storage: Membrane.RTC.Engine.Endpoint.HLS.HLSConfig.default_storage(directory),
        should_stop?: should_stop?
      }
    end

    hls_mode = if input_type == :muxed, do: :muxed_av, else: :separate_av

    %HLS{
      rtc_engine: rtc_engine,
      owner: self(),
      output_directory: output_dir,
      synchronize_tracks?: false,
      mixer_config: mixer_config,
      hls_config: %HLSConfig{
        mode: :vod,
        target_window_duration: :infinity,
        hls_mode: hls_mode,
        segment_duration: Membrane.Time.seconds(3),
        storage: storage
      }
    }
  end

  defp check_muxed_hls_playlist(output_dir, segments_number, manifests_number) do
    output_files = File.ls!(output_dir) |> Enum.sort()

    manifest_files = Enum.filter(output_files, &is_m3u8_file?(&1))
    muxed_manifest = find_muxed_manifest(manifest_files, output_dir)
    headers = find_headers(output_files)

    segment_sizes = extract_segment_sizes(muxed_manifest, output_dir, segments_number)

    assert Enum.count(manifest_files) == manifests_number

    # There should exactly one header for each manifest, except for master manifest
    assert Enum.count(headers) == manifests_number - 1

    assert Enum.count(segment_sizes) == segments_number
    assert Enum.all?(segment_sizes, &(&1 > 0))
  end

  defp check_separate_hls_playlist(output_dir, segments_number, manifests_number) do
    output_files = File.ls!(output_dir) |> Enum.sort()

    manifest_files = Enum.filter(output_files, &is_m3u8_file?(&1))
    stream_manifests = find_stream_manifests(manifest_files, output_dir)
    headers = find_headers(output_files)

    segment_sizes =
      Enum.map(stream_manifests, &extract_segment_sizes(&1, output_dir, segments_number))

    assert Enum.count(manifest_files) == manifests_number

    # There should exactly one header for each manifest, except for master manifest
    assert Enum.count(headers) == manifests_number - 1

    Enum.each(segment_sizes, fn segment_sizes ->
      assert Enum.count(segment_sizes) == segments_number
      assert Enum.all?(segment_sizes, &(&1 > 0))
    end)
  end

  defp is_m3u8_file?(filename) do
    String.match?(filename, ~r/.*\.m3u8$/)
  end

  defp find_muxed_manifest(manifest_files, output_dir) do
    manifest_files
    |> Enum.find(nil, &String.match?(&1, ~r/^(?!index).*$/))
    |> then(&Path.join(output_dir, &1))
  end

  defp find_stream_manifests(manifest_files, output_dir) do
    manifest_files
    |> Enum.filter(&String.match?(&1, ~r/^(?!index).*$/))
    |> Enum.map(&Path.join(output_dir, &1))
  end

  defp find_headers(files) do
    Enum.filter(files, &String.match?(&1, ~r/.*header.*$/))
  end

  defp extract_segment_sizes(manifest_path, output_dir, segments_number) do
    File.stream!(manifest_path)
    |> Enum.filter(&String.match?(&1, ~r/.*\.m4s/))
    |> Enum.take(segments_number)
    |> Enum.map(&String.trim/1)
    |> Enum.map(&Path.join(output_dir, &1))
    |> Enum.map(&File.stat!/1)
    |> Enum.map(& &1.size)
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

    %FileSourceEndpoint{
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

    %FileSourceEndpoint{
      rtc_engine: rtc_engine,
      file_path: Path.join(@fixtures_dir, "audio.aac"),
      track: audio_track,
      ssrc: 2345,
      payload_type: 108
    }
  end
end
