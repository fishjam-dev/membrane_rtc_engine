defmodule Membrane.RTC.Engine.Endpoint.Recording.ReporterTest do
  use ExUnit.Case, async: true

  import MockTrack

  alias Membrane.RTC.Engine.Endpoint.Recording.Reporter

  @sec_to_ns 10 ** 9

  @video_clock_rate 90_000
  @audio_clock_rate 48_000

  setup do
    id = UUID.uuid4()
    {:ok, pid} = Reporter.start(id)
    on_exit(fn -> Reporter.stop(pid) end)

    {:ok, %{reporter: pid, id: id}}
  end

  test "Add one track and then end track", %{reporter: reporter, id: id} do
    filename = "track_1.msr"

    track =
      %{encoding: encoding, clock_rate: clock_rate, metadata: metadata, origin: origin} =
      create_track(:video)

    start_timestamp = 0

    :ok = Reporter.add_track(reporter, track, filename, start_timestamp)
    :ok = Reporter.start_timestamp(reporter, track.id, start_timestamp)

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename => %{
                 type: :video,
                 encoding: ^encoding,
                 offset: ^start_timestamp,
                 start_timestamp: ^start_timestamp,
                 end_timestamp: ^start_timestamp,
                 clock_rate: ^clock_rate,
                 metadata: ^metadata,
                 origin: ^origin
               }
             }
           } = Reporter.get_report(reporter)

    end_timestamp = 10
    :ok = Reporter.end_timestamp(reporter, track.id, end_timestamp)

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename => %{
                 type: :video,
                 encoding: ^encoding,
                 offset: ^start_timestamp,
                 start_timestamp: ^start_timestamp,
                 end_timestamp: ^end_timestamp,
                 clock_rate: ^clock_rate,
                 metadata: ^metadata,
                 origin: ^origin
               }
             }
           } = Reporter.get_report(reporter)
  end

  test "Add multiple tracks", %{reporter: reporter, id: id} do
    filename_1 = "track_1.msr"
    filename_2 = "track_2.msr"

    track_1 =
      %{encoding: encoding_1, clock_rate: clock_rate_1, metadata: metadata_1, origin: origin_1} =
      create_track(:video)

    track_2 =
      %{encoding: encoding_2, clock_rate: clock_rate_2, metadata: metadata_2, origin: origin_2} =
      create_track(:audio)

    start_timestamp_1 = 0
    start_timestamp_2 = 10

    :ok = Reporter.add_track(reporter, track_1, filename_1, start_timestamp_1)
    :ok = Reporter.start_timestamp(reporter, track_1.id, start_timestamp_1)

    :ok = Reporter.add_track(reporter, track_2, filename_2, start_timestamp_2)
    :ok = Reporter.start_timestamp(reporter, track_2.id, start_timestamp_2)

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename_1 => %{
                 type: :video,
                 encoding: ^encoding_1,
                 offset: ^start_timestamp_1,
                 start_timestamp: ^start_timestamp_1,
                 end_timestamp: ^start_timestamp_1,
                 clock_rate: ^clock_rate_1,
                 metadata: ^metadata_1,
                 origin: ^origin_1
               },
               ^filename_2 => %{
                 type: :audio,
                 encoding: ^encoding_2,
                 offset: ^start_timestamp_2,
                 start_timestamp: ^start_timestamp_2,
                 end_timestamp: ^start_timestamp_2,
                 clock_rate: ^clock_rate_2,
                 metadata: ^metadata_2,
                 origin: ^origin_2
               }
             }
           } = Reporter.get_report(reporter)

    end_timestamp_1 = 30
    Reporter.end_timestamp(reporter, track_1.id, end_timestamp_1)

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename_1 => %{
                 type: :video,
                 encoding: ^encoding_1,
                 offset: ^start_timestamp_1,
                 start_timestamp: ^start_timestamp_1,
                 end_timestamp: ^end_timestamp_1,
                 clock_rate: ^clock_rate_1,
                 metadata: ^metadata_1,
                 origin: ^origin_1
               },
               ^filename_2 => %{
                 type: :audio,
                 encoding: ^encoding_2,
                 offset: ^start_timestamp_2,
                 start_timestamp: ^start_timestamp_2,
                 end_timestamp: ^start_timestamp_2,
                 clock_rate: ^clock_rate_2,
                 metadata: ^metadata_2,
                 origin: ^origin_2
               }
             }
           } = Reporter.get_report(reporter)

    end_timestamp_2 = 40
    Reporter.end_timestamp(reporter, track_2.id, end_timestamp_2)

    assert %{
             recording_id: ^id,
             tracks: %{
               ^filename_1 => %{
                 type: :video,
                 encoding: ^encoding_1,
                 offset: ^start_timestamp_1,
                 start_timestamp: ^start_timestamp_1,
                 end_timestamp: ^end_timestamp_1,
                 clock_rate: ^clock_rate_1,
                 metadata: ^metadata_1,
                 origin: ^origin_1
               },
               ^filename_2 => %{
                 type: :audio,
                 encoding: ^encoding_2,
                 offset: ^start_timestamp_2,
                 start_timestamp: ^start_timestamp_2,
                 end_timestamp: ^end_timestamp_2,
                 clock_rate: ^clock_rate_2,
                 metadata: ^metadata_2,
                 origin: ^origin_2
               }
             }
           } = Reporter.get_report(reporter)
  end

  describe "RTCP synchronization" do
    # This test verifies that all tracks correctly receive and handle RTCP reports.
    # It creates RTCP reports using predefined wall clock times and a specific clock rate, and then applies them.
    # After calculating the `start_timestamp_wallclock` values, the test checks the updated report to ensure that the `start_timestamp_wallclock` values of all tracks have been correctly calculated.

    test "all tracks have RTCP reports", %{reporter: reporter} do
      [track_1, track_2] =
        [
          %{type: :video, filename: "track_1.msr", start_timestamp: 0},
          %{type: :audio, filename: "track_2.msr", start_timestamp: 10}
        ]
        |> Enum.map(&add_track(reporter, &1))

      wallclock_1 = 100 * @sec_to_ns
      wallclock_2 = 120 * @sec_to_ns

      rtcp_2 = create_rtcp_report(track_2.start_timestamp + @audio_clock_rate, wallclock_2)
      rtcp_1 = create_rtcp_report(track_1.start_timestamp + @video_clock_rate, wallclock_1)

      :ok = Reporter.rtcp_packet(reporter, track_1.id, rtcp_1)
      :ok = Reporter.rtcp_packet(reporter, track_2.id, rtcp_2)

      wallclock_1 = wallclock_1 - @sec_to_ns
      wallclock_2 = wallclock_2 - @sec_to_ns

      # after sending rtcp
      assert %{
               tracks: %{
                 "track_1.msr" => %{start_timestamp_wallclock: ^wallclock_1},
                 "track_2.msr" => %{start_timestamp_wallclock: ^wallclock_2}
               }
             } = Reporter.get_report(reporter)
    end

    test "only two tracks have RTCP reports", %{reporter: reporter, id: id} do
      [_track_1, _track_2, track_3, track_4] =
        [
          %{type: :video, filename: "track_1.msr", start_timestamp: 0},
          %{type: :audio, filename: "track_2.msr", start_timestamp: 10},
          %{type: :audio, filename: "track_3.msr", start_timestamp: 20},
          %{type: :audio, filename: "track_4.msr", start_timestamp: 30}
        ]
        |> Enum.map(&add_track(reporter, &1))

      wallclock_3 = 100 * @sec_to_ns
      wallclock_4 = 120 * @sec_to_ns

      rtcp_3 = create_rtcp_report(track_3.start_timestamp, wallclock_3)
      rtcp_4 = create_rtcp_report(track_4.start_timestamp, wallclock_4)

      :ok = Reporter.rtcp_packet(reporter, track_3.id, rtcp_3)
      :ok = Reporter.rtcp_packet(reporter, track_4.id, rtcp_4)

      # after sending rtcp
      assert %{
               recording_id: ^id,
               tracks: %{
                 "track_3.msr" => %{start_timestamp_wallclock: ^wallclock_3},
                 "track_4.msr" => %{start_timestamp_wallclock: ^wallclock_4}
               }
             } = Reporter.get_report(reporter)

      refute match?(
               %{
                 recording_id: ^id,
                 tracks: %{
                   "track_1.msr" => %{start_timestamp_wallclock: _time_1},
                   "track_2.msr" => %{start_timestamp_wallclock: _time_2}
                 }
               },
               Reporter.get_report(reporter)
             )
    end
  end

  defp add_track(reporter, %{type: type, filename: filename, start_timestamp: start_timestamp}) do
    track = create_track(type)

    start_timestamp = start_timestamp * track.clock_rate
    offset = start_timestamp

    :ok = Reporter.add_track(reporter, track, filename, offset)
    :ok = Reporter.start_timestamp(reporter, track.id, start_timestamp)

    %{
      type: type,
      filename: filename,
      start_timestamp: start_timestamp,
      offset: offset,
      id: track.id
    }
  end
end
