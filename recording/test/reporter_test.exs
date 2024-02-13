defmodule Membrane.RTC.Engine.Endpoint.Recording.ReporterTest do
  use ExUnit.Case, async: true

  import MockTrack

  alias Membrane.RTC.Engine.Endpoint.Recording.Reporter

  setup do
    id = UUID.uuid4()
    {:ok, pid} = Reporter.start(id)
    on_exit(fn -> Reporter.stop(pid) end)

    {:ok, %{reporter: pid, id: id}}
  end

  test "Add one track", %{reporter: reporter, id: id} do
    filename = "track_1.msr"

    track =
      %{encoding: encoding, clock_rate: clock_rate, metadata: metadata} = create_track(:video)

    offset = 0

    :ok = Reporter.add_track(reporter, track, filename, offset)

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename => %{
                 type: :video,
                 encoding: ^encoding,
                 offset: ^offset,
                 clock_rate: ^clock_rate,
                 metadata: ^metadata
               }
             }
           } = Reporter.get_report(reporter)
  end

  test "Add multiple tracks", %{reporter: reporter, id: id} do
    filename_1 = "track_1.msr"
    filename_2 = "track_2.msr"

    track_1 =
      %{encoding: encoding_1, clock_rate: clock_rate_1, metadata: metadata_1} =
      create_track(:video)

    track_2 =
      %{encoding: encoding_2, clock_rate: clock_rate_2, metadata: metadata_2} =
      create_track(:audio)

    offset_1 = 0
    offset_2 = 10

    :ok = Reporter.add_track(reporter, track_1, filename_1, offset_1)
    :ok = Reporter.add_track(reporter, track_2, filename_2, offset_2)

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename_1 => %{
                 type: :video,
                 encoding: ^encoding_1,
                 offset: ^offset_1,
                 clock_rate: ^clock_rate_1,
                 metadata: ^metadata_1
               },
               ^filename_2 => %{
                 type: :audio,
                 encoding: ^encoding_2,
                 offset: ^offset_2,
                 clock_rate: ^clock_rate_2,
                 metadata: ^metadata_2
               }
             }
           } = Reporter.get_report(reporter)
  end
end
