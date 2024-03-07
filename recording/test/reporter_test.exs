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

  test "Add one track and then end track", %{reporter: reporter, id: id} do
    filename = "track_1.msr"

    track =
      %{encoding: encoding, clock_rate: clock_rate, metadata: metadata} = create_track(:video)

    start_timestamp = Reporter.get_timestamp()

    :ok = Reporter.add_track(reporter, track, filename, start_timestamp)

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename => %{
                 type: :video,
                 encoding: ^encoding,
                 start_timestamp: 0,
                 end_timestamp: end_timestamp,
                 clock_rate: ^clock_rate,
                 metadata: ^metadata
               }
             }
           } = Reporter.get_report(reporter)

    assert end_timestamp >= 0

    end_timestamp = Reporter.get_timestamp()
    :ok = Reporter.end_track(reporter, track, end_timestamp)

    end_timestamp = end_timestamp - start_timestamp

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename => %{
                 type: :video,
                 encoding: ^encoding,
                 start_timestamp: 0,
                 end_timestamp: ^end_timestamp,
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

    start_timestamp_1 = Reporter.get_timestamp()
    start_timestamp_2 = 10

    :ok = Reporter.add_track(reporter, track_1, filename_1, start_timestamp_1)
    :ok = Reporter.add_track(reporter, track_2, filename_2, start_timestamp_1 + start_timestamp_2)

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename_1 => %{
                 type: :video,
                 encoding: ^encoding_1,
                 start_timestamp: 0,
                 end_timestamp: end_timestamp_1,
                 clock_rate: ^clock_rate_1,
                 metadata: ^metadata_1
               },
               ^filename_2 => %{
                 type: :audio,
                 encoding: ^encoding_2,
                 start_timestamp: ^start_timestamp_2,
                 end_timestamp: end_timestamp_2,
                 clock_rate: ^clock_rate_2,
                 metadata: ^metadata_2
               }
             }
           } = Reporter.get_report(reporter)

    assert end_timestamp_1 >= 0
    assert end_timestamp_2 >= 0

    end_timestamp_1 = Reporter.get_timestamp()
    Reporter.end_track(reporter, track_1, end_timestamp_1)
    end_timestamp_1 = end_timestamp_1 - start_timestamp_1

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename_1 => %{
                 type: :video,
                 encoding: ^encoding_1,
                 start_timestamp: 0,
                 end_timestamp: ^end_timestamp_1,
                 clock_rate: ^clock_rate_1,
                 metadata: ^metadata_1
               },
               ^filename_2 => %{
                 type: :audio,
                 encoding: ^encoding_2,
                 start_timestamp: ^start_timestamp_2,
                 end_timestamp: end_timestamp_2,
                 clock_rate: ^clock_rate_2,
                 metadata: ^metadata_2
               }
             }
           } = Reporter.get_report(reporter)

    assert end_timestamp_2 >= 0
    end_timestamp_2 = Reporter.get_timestamp()
    Reporter.end_track(reporter, track_2, end_timestamp_2)
    end_timestamp_2 = end_timestamp_2 - start_timestamp_1

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename_1 => %{
                 type: :video,
                 encoding: ^encoding_1,
                 start_timestamp: 0,
                 end_timestamp: ^end_timestamp_1,
                 clock_rate: ^clock_rate_1,
                 metadata: ^metadata_1
               },
               ^filename_2 => %{
                 type: :audio,
                 encoding: ^encoding_2,
                 start_timestamp: ^start_timestamp_2,
                 end_timestamp: ^end_timestamp_2,
                 clock_rate: ^clock_rate_2,
                 metadata: ^metadata_2
               }
             }
           } = Reporter.get_report(reporter)
  end
end
