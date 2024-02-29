defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.FileTest do
  use ExUnit.Case, async: true

  import MockTrack

  alias Membrane.RTC.Engine.Endpoint.Recording.{Reporter, Storage}

  @tag :tmp_dir
  test "create file sink", %{tmp_dir: output_dir} do
    filename = "track_1"

    config = %{
      track: %{},
      path_prefix: output_dir,
      filename: filename
    }

    location = Path.join(output_dir, filename)

    assert %Membrane.File.Sink{location: ^location} = Storage.File.get_sink(config, nil)

    assert output_dir |> Path.join(config.filename) |> File.exists?()
  end

  @tag :tmp_dir
  test "save report", %{tmp_dir: output_dir} do
    {:ok, reporter} = Reporter.start(UUID.uuid4())

    filename = "track_1.msr"
    track = create_track(:video)
    offset = 0

    :ok = Reporter.add_track(reporter, track, filename, offset)

    report_json =
      reporter
      |> Reporter.get_report()
      |> Jason.encode!()

    config = %{
      object: report_json,
      path_prefix: output_dir,
      filename: "report.json"
    }

    :ok = Storage.File.save_object(config, nil)

    assert output_dir |> Path.join("report.json") |> File.exists?()

    saved_report = output_dir |> Path.join("report.json") |> File.read!()

    assert saved_report == report_json

    :ok = Reporter.stop(reporter)
  end
end
