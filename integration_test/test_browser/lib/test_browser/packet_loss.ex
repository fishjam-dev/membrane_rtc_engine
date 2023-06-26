defmodule TestBrowser.PacketLoss do
  import TestBrowser.Utils

  # in milliseconds
  @warmup_time 5000
  @packet_loss_warmup_time 5000

  @start_button "start-all"
  @simulcast_inbound_stats "simulcast-inbound-stats"
  @browser_options %{count: 1, headless: true, args: ["--ignore-certificate-errors"]}

  @stats_number 15
  @stats_interval 1_000

  def run_test() do
    server = System.get_env("SERVER_HOSTNAME", "localhost")
    server_receiver = {TestVideoroom.TestResultReceiver, String.to_atom(server <> "@" <> server)}
    stats_task = Task.async(fn -> receive_stats(server_receiver) end)

    hostname = with {:ok, hostname} <- :inet.gethostname(), do: to_string(hostname)

    mustang_options = %{
      target_url: "https://" <> server <> ":4005",
      warmup_time: @warmup_time,
      start_button: @start_button,
      receiver: stats_task.pid,
      server: server_receiver,
      actions: [
        {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
        {:wait, @packet_loss_warmup_time},
        {:notify_server, :enable_packet_loss},
        {:wait, @packet_loss_warmup_time},
        {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
         tag: :after_applying_packet_loss_on_one_user}
      ],
      id: hostname
    }

    Task.async(fn ->
      Stampede.start({TestBrowser.Mustang, mustang_options}, @browser_options)
    end)
    |> Task.await(:infinity)

    Task.await(stats_task, :infinity)

    :ok
  end
end
