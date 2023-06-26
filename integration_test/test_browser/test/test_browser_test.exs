defmodule TestBrowserTest do
  use ExUnit.Case

  import TestBrowser.Utils

  # in milliseconds
  @test_warmup_time 5000
  @browser_warmup_time 5000
  @packet_loss_warmup_time 5000
  @max_test_duration 400_000

  @start_button "start-all"
  @simulcast_inbound_stats "simulcast-inbound-stats"
  @simulcast_outbound_stats "simulcast-outbound-stats"
  @browser_options %{count: 1, headless: true, args: ["--ignore-certificate-errors"]}

  @stats_number 15
  @stats_interval 1_000

  @tag timeout: @max_test_duration
  test "run browser for packet loss test" do
    # Overhead for media server startup
    Process.sleep(@test_warmup_time)

    server = System.get_env("SERVER_HOSTNAME", "localhost")
    server_receiver = {TestVideoroom.TestResultReceiver, String.to_atom(server <> "@" <> server)}
    stats_task = Task.async(fn -> receive_stats(server_receiver) end)

    hostname = with {:ok, hostname} <- :inet.gethostname(), do: to_string(hostname)

    mustang_options = %{
      target_url: "https://" <> server <> ":4005",
      warmup_time: @browser_warmup_time,
      start_button: @start_button,
      receiver: stats_task.pid,
      server: server_receiver,
      actions: [
        {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
        {:wait, @packet_loss_warmup_time},
        {:notify_server, :enable_packet_loss},
        {:wait, @packet_loss_warmup_time},
        {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
          tag: :after_applying_packet_loss_on_one_user},

        # Get local stream stats so that we know our stream ID
        {:get_stats, @simulcast_outbound_stats, 1, 0, tag: :local_stream_stats}
      ],
      id: hostname
    }

    Task.async(fn ->
      Stampede.start({TestBrowser.Mustang, mustang_options}, @browser_options)
    end)
    |> Task.await(:infinity)

    Task.await(stats_task, :infinity)
  end
end
