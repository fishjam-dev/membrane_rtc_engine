defmodule Membrane.RTC.RecordingEndpointTest do
  use ExUnit.Case

  import FileEndpointGenerator
  import Membrane.ChildrenSpec
  import Membrane.Testing.Assertions

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.Recording, as: RecordingEndpoint
  alias Membrane.RTC.Engine.Endpoint.Recording.Storage
  alias Membrane.RTC.Engine.Message.{EndpointRemoved, TrackAdded, TrackRemoved}

  @fixtures_dir "./test/fixtures/"
  @report_filename "report.json"
  @tracks_added_delay 500
  @tracks_removed_delay 15_000
  @report_delay 1_000

  setup do
    options = [id: "test_rtc"]
    {:ok, pid} = Engine.start_link(options, [])
    Engine.register(pid, self())
    on_exit(fn -> Engine.terminate(pid) end)

    [rtc_engine: pid]
  end

  @tag :tmp_dir
  test "creates correct recording, one input", %{rtc_engine: rtc_engine, tmp_dir: output_dir} do
    recording_endpoint_id = "recording-endpoint"
    video_file_endpoint_id = "video-file-endpoint"

    video_file_path = Path.join(@fixtures_dir, "recorded_video.h264")
    deserialized_file_path = Path.join(output_dir, "deserialized.h264")
    report_file_path = Path.join(output_dir, @report_filename)

    recording_endpoint = create_recording_endpoint(rtc_engine, output_dir)
    video_file_endpoint = create_video_file_endpoint(rtc_engine, video_file_path)

    :ok = Engine.add_endpoint(rtc_engine, recording_endpoint, id: recording_endpoint_id)
    :ok = Engine.add_endpoint(rtc_engine, video_file_endpoint, id: video_file_endpoint_id)

    assert_receive %TrackAdded{endpoint_id: ^video_file_endpoint_id}, @tracks_added_delay
    assert_receive %TrackRemoved{endpoint_id: ^video_file_endpoint_id}, @tracks_removed_delay

    [filename] = File.ls!(output_dir)

    video_deserializer =
      create_video_deserializer(%{
        source: Path.join(output_dir, filename),
        output: deserialized_file_path,
        owner: self(),
        type: :video
      })

    pipeline = Membrane.Testing.Pipeline.start_link_supervised!(spec: video_deserializer)

    assert_end_of_stream(pipeline, :sink)

    assert File.read!(video_file_path) == File.read!(deserialized_file_path)

    Engine.remove_endpoint(rtc_engine, recording_endpoint_id)
    assert_receive %EndpointRemoved{endpoint_id: ^recording_endpoint_id}

    await_report(report_file_path)
    validate_report(report_file_path)
  end

  @tag :tmp_dir
  test "creates correct recording, multiple inputs", %{
    rtc_engine: rtc_engine,
    tmp_dir: output_dir
  } do
    recording_endpoint_id = "recording-endpoint"
    audio_file_endpoint_id = "audio-file-endpoint"
    video_file_endpoint_id = "video-file-endpoint"

    audio_file_path = Path.join(@fixtures_dir, "audio.aac")
    video_file_path = Path.join(@fixtures_dir, "recorded_video.h264")
    report_file_path = Path.join(output_dir, @report_filename)

    deserialized_audio_path = Path.join(output_dir, "deserialized_audio.aac")
    deserialized_video_path = Path.join(output_dir, "deserialized_video.h264")

    recording_endpoint = create_recording_endpoint(rtc_engine, output_dir)
    audio_file_endpoint = create_audio_file_endpoint(rtc_engine, audio_file_path)
    video_file_endpoint = create_video_file_endpoint(rtc_engine, video_file_path)

    :ok = Engine.add_endpoint(rtc_engine, recording_endpoint, id: recording_endpoint_id)
    :ok = Engine.add_endpoint(rtc_engine, video_file_endpoint, id: video_file_endpoint_id)
    :ok = Engine.add_endpoint(rtc_engine, audio_file_endpoint, id: audio_file_endpoint_id)

    assert_receive %TrackAdded{endpoint_id: ^video_file_endpoint_id}, @tracks_added_delay
    assert_receive %TrackAdded{endpoint_id: ^audio_file_endpoint_id}, @tracks_added_delay
    assert_receive %TrackRemoved{endpoint_id: ^video_file_endpoint_id}, @tracks_removed_delay
    assert_receive %TrackRemoved{endpoint_id: ^audio_file_endpoint_id}

    filenames = File.ls!(output_dir)
    audio_file = Enum.find(filenames, fn filename -> String.starts_with?(filename, "audio") end)
    video_file = Enum.find(filenames, fn filename -> String.starts_with?(filename, "video") end)

    audio_deserializer =
      create_audio_deserializer(%{
        source: Path.join(output_dir, audio_file),
        output: deserialized_audio_path,
        owner: self()
      })

    video_deserializer =
      create_video_deserializer(%{
        source: Path.join(output_dir, video_file),
        output: deserialized_video_path,
        owner: self()
      })

    audio_pipeline = Membrane.Testing.Pipeline.start_link_supervised!(spec: audio_deserializer)
    video_pipeline = Membrane.Testing.Pipeline.start_link_supervised!(spec: video_deserializer)

    assert_end_of_stream(audio_pipeline, :sink)
    assert_end_of_stream(video_pipeline, :sink)

    assert deserialized_audio_path |> File.read!() |> byte_size() > 0
    assert deserialized_video_path |> File.read!() |> byte_size() > 0

    Engine.remove_endpoint(rtc_engine, recording_endpoint_id)
    assert_receive %EndpointRemoved{endpoint_id: ^recording_endpoint_id}

    await_report(report_file_path)
    validate_report(report_file_path)
  end

  defp create_recording_endpoint(rtc_engine, output_dir) do
    %RecordingEndpoint{
      rtc_engine: rtc_engine,
      recording_id: "id",
      output_dir: output_dir,
      stores: [Storage.File]
    }
  end

  defp create_video_deserializer(opts) do
    [
      child(:source, %Membrane.File.Source{location: opts.source})
      |> child(:deserializer, Membrane.Stream.Deserializer)
      |> child(:rtp, %Membrane.RTP.DepayloaderBin{
        depayloader: Membrane.RTP.H264.Depayloader,
        clock_rate: 90_000
      })
      |> child(:parser, %Membrane.H264.Parser{
        generate_best_effort_timestamps: %{framerate: {60, 1}}
      })
      |> child(:sink, %Membrane.File.Sink{location: opts.output})
    ]
  end

  defp create_audio_deserializer(opts) do
    [
      child(:source, %Membrane.File.Source{location: opts.source})
      |> child(:deserializer, Membrane.Stream.Deserializer)
      |> child(:rtp, %Membrane.RTP.DepayloaderBin{
        depayloader: Membrane.RTP.Opus.Depayloader,
        clock_rate: 48_000
      })
      |> child(:sink, %Membrane.File.Sink{location: opts.output})
    ]
  end

  defp await_report(report_path) do
    # ms
    interval = 100
    iterations = div(@report_delay, interval)

    Enum.any?(1..iterations, fn _ ->
      Process.sleep(interval)
      File.exists?(report_path)
    end)
  end

  defp validate_report(report_path) do
    assert report_path |> File.read!() |> byte_size() > 0
  end
end