if Enum.all?(
     Membrane.RTC.Engine.MixProject.rtsp_endpoint_deps(),
     &Code.ensure_loaded?/1
   ) do
  defmodule Membrane.RTC.Engine.Endpoint.RTSP.ConnectionManager do
    @moduledoc false
    use Connection

    require Membrane.Logger

    alias Membrane.RTSP

    @delay 15_000
    @keep_alive_interval 15_000

    defmodule ConnectionStatus do
      @moduledoc false
      use Bunch.Access

      @type t :: %__MODULE__{
              stream_uri: binary(),
              rtsp_session: pid(),
              endpoint: pid(),
              keep_alive: pid(),
              endpoint_options: map()
            }

      @enforce_keys [
        :stream_uri,
        :endpoint,
        :endpoint_options
      ]

      defstruct @enforce_keys ++
                  [
                    :rtsp_session,
                    :keep_alive
                  ]
    end

    @spec start_link(Keyword.t()) :: GenServer.on_start()
    def start_link(args) do
      Membrane.Logger.debug("ConnectionManager: start_link, args: #{inspect(args)}")

      Connection.start_link(__MODULE__, args, name: ConnectionManager)
    end

    @impl true
    def init(stream_uri: stream_uri, port: port, endpoint: endpoint) do
      Membrane.Logger.debug("ConnectionManager: Initializing")

      {:connect, :init,
       %ConnectionStatus{
         stream_uri: stream_uri,
         endpoint_options: %{
           port: port,
           rtpmap: nil,
           fmtp: nil,
           control: nil,
           server_port: nil
         },
         endpoint: endpoint
       }}
    end

    @impl true
    def connect(_info, %ConnectionStatus{} = connection_status) do
      Membrane.Logger.debug("ConnectionManager: Connecting")

      rtsp_session = start_rtsp_session(connection_status)
      connection_status = %{connection_status | rtsp_session: rtsp_session}

      if is_nil(rtsp_session) do
        {:backoff, @delay, connection_status}
      else
        with {:ok, connection_status} <- get_rtsp_description(connection_status),
             {:ok, connection_status} <- setup_rtsp_connection(connection_status),
             {:ok, connection_status} <- start_keep_alive(connection_status),
             :ok <- play(connection_status) do
          Membrane.Logger.warn(~s"""
          ConnectionManager processes:
            RTSP session: #{inspect(connection_status.rtsp_session)},
            Endpoint: #{inspect(connection_status.endpoint)},
            RTSP keep alive: #{inspect(connection_status.keep_alive)}
          """)

          send(
            connection_status.endpoint,
            {:rtsp_setup_complete, connection_status.endpoint_options}
          )

          {:ok, connection_status}
        else
          {:error, :unauthorized} ->
            Membrane.Logger.warn(
              "ConnectionManager: Unauthorized. Attempting immediate reconnect..."
            )

            {:backoff, 0, connection_status}

          {:error, error_message} ->
            Membrane.Logger.warn(
              "ConnectionManager: Connection failed: #{inspect(error_message)}"
            )

            {:backoff, @delay, connection_status}
        end
      end
    end

    @impl true
    def disconnect(message, %ConnectionStatus{} = connection_status) do
      Membrane.Logger.debug("ConnectionManager: Disconnecting: #{message}")

      kill_children(connection_status)

      connection_status = %{
        connection_status
        | rtsp_session: nil,
          keep_alive: nil
      }

      case message do
        :reload ->
          {:connect, :reload, connection_status}

        {:error, error_message} ->
          Membrane.Logger.error("ConnectionManager: Error: #{inspect(error_message)}")
          {:backoff, @delay, connection_status}
      end
    end

    defp kill_children(%ConnectionStatus{keep_alive: keep_alive, rtsp_session: rtsp_session}) do
      if !is_nil(keep_alive) and Process.alive?(keep_alive),
        do: GenServer.stop(keep_alive, :normal)

      if !is_nil(rtsp_session) and Process.alive?(rtsp_session), do: RTSP.close(rtsp_session)
    end

    @impl true
    def handle_info(
          {:DOWN, _ref, :process, pid, reason},
          %ConnectionStatus{
            rtsp_session: rtsp_session,
            keep_alive: keep_alive
          } = connection_status
        )
        when reason != :normal do
      Membrane.Logger.warn("ConnectionManager: Received DOWN message from #{inspect(pid)}")

      Membrane.Logger.warn(~s"""
      ConnectionManager processes:
        RTSP session: #{inspect(rtsp_session)},
        RTSP keep alive: #{inspect(keep_alive)}
      """)

      case pid do
        ^rtsp_session ->
          Membrane.Logger.error("ConnectionManager: RTSP session crashed")

        ^keep_alive ->
          Membrane.Logger.error("ConnectionManager: Keep_alive process crashed")

        process ->
          Membrane.Logger.error("ConnectionManager: #{inspect(process)} process crashed")
      end

      {:disconnect, :reload, connection_status}
    end

    @impl true
    def handle_info({:DOWN, _ref, :process, _pid, reason}, connection_status)
        when reason == :normal do
      {:noreply, connection_status}
    end

    @impl true
    def handle_info({:EXIT, _from, reason}, connection_status) do
      {:disconnect, {:error, reason}, connection_status}
    end

    defp start_rtsp_session(%ConnectionStatus{rtsp_session: nil, stream_uri: stream_uri}) do
      case RTSP.start(stream_uri) do
        {:ok, session} ->
          Process.monitor(session)
          session

        {:error, error} ->
          Membrane.Logger.warn(
            "ConnectionManager: Starting RTSP session failed - #{inspect(error)}"
          )

          nil
      end
    end

    defp start_rtsp_session(%ConnectionStatus{rtsp_session: rtsp_session}) do
      rtsp_session
    end

    defp get_rtsp_description(%ConnectionStatus{rtsp_session: rtsp_session} = connection_status) do
      Membrane.Logger.debug("ConnectionManager: Setting up RTSP description")

      case RTSP.describe(rtsp_session) do
        {:ok, %{status: 200} = response} ->
          attributes = get_video_attributes(response)

          connection_status =
            connection_status
            |> put_in([:endpoint_options, :control], get_attribute(attributes, "control", ""))
            |> put_in([:endpoint_options, :fmtp], get_attribute(attributes, ExSDP.Attribute.FMTP))
            |> put_in(
              [:endpoint_options, :rtpmap],
              get_attribute(attributes, ExSDP.Attribute.RTPMapping)
            )

          {:ok, connection_status}

        {:ok, %{status: 401}} ->
          {:error, :unauthorized}

        _result ->
          {:error, :getting_rtsp_description_failed}
      end
    end

    defp setup_rtsp_connection(
           %ConnectionStatus{
             rtsp_session: rtsp_session,
             endpoint_options: endpoint_options
           } = connection_status
         ) do
      Membrane.Logger.debug("ConnectionManager: Setting up RTSP connection")

      case RTSP.setup(rtsp_session, "/#{endpoint_options.control}", [
             {"Transport", "RTP/AVP;unicast;client_port=#{endpoint_options.port}"}
           ]) do
        {:ok, %{status: 200, headers: headers}} ->
          # Parse server port to be able to combat NAT later
          server_port = parse_server_port(headers)

          {:ok, put_in(connection_status, [:endpoint_options, :server_port], server_port)}

        result ->
          Membrane.Logger.debug(
            "ConnectionManager: Setting up RTSP connection failed: #{inspect(result)}"
          )

          {:error, :setting_up_sdp_connection_failed}
      end
    end

    defp play(%ConnectionStatus{rtsp_session: rtsp_session, endpoint: _endpoint}) do
      Membrane.Logger.debug("ConnectionManager: Setting RTSP on play mode")

      case RTSP.play(rtsp_session) do
        {:ok, %{status: 200}} ->
          :ok

        _result ->
          {:error, :play_rtsp_failed}
      end
    end

    defp start_keep_alive(%ConnectionStatus{rtsp_session: rtsp_session} = connection_status) do
      Membrane.Logger.debug("ConnectionManager: Starting Keep alive process")

      {:ok, keep_alive} =
        Task.start(fn ->
          rtsp_keep_alive(rtsp_session)
        end)

      Process.monitor(keep_alive)

      {:ok, %{connection_status | keep_alive: keep_alive}}
    end

    defp rtsp_keep_alive(rtsp_session) do
      case RTSP.get_parameter(rtsp_session) do
        {:ok, %RTSP.Response{status: 200}} ->
          Process.sleep(@keep_alive_interval)
          rtsp_keep_alive(rtsp_session)

        error ->
          Membrane.Logger.warn("RTSP ping failed: #{inspect(error)}")
          Process.exit(self(), :connection_failed)
      end
    end

    defp get_video_attributes(%{body: %ExSDP{media: media_list}}) do
      media_list |> Enum.find(fn elem -> elem.type == :video end)
    end

    defp get_attribute(video_attributes, attribute, default \\ nil) do
      case ExSDP.Media.get_attribute(video_attributes, attribute) do
        {^attribute, value} -> value
        %^attribute{} = value -> value
        _other -> default
      end
    end

    defp parse_server_port(headers) do
      Enum.find_value(headers, fn entry ->
        case entry do
          {"Transport", value} -> value
          _other -> false
        end
      end)
      |> String.split(";")
      |> Enum.find_value(fn entry ->
        String.split(entry, "=")
        |> List.to_tuple()
        |> case do
          {"server_port", port_range} ->
            [range_start | _range_end] = String.split(port_range, "-")
            String.to_integer(range_start)

          _other ->
            false
        end
      end)
    end
  end
end
