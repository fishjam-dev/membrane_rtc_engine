defmodule WebRTCToHLS.Stream do
  @moduledoc false

  use GenServer

  require Membrane.Logger

  alias Membrane.ICE.TURNManager

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.{HLS, WebRTC}
  alias Membrane.RTC.Engine.Endpoint.HLS.{HLSConfig, MixerConfig}
  alias Membrane.RTC.Engine.Message.{EndpointCrashed, EndpointMessage, EndpointRemoved}

  alias Membrane.WebRTC.Extension.{Mid, Rid}
  alias Membrane.WebRTC.Track.Encoding

  @mix_env Mix.env()

  def start(channel_pid, peer_id) do
    GenServer.start(__MODULE__, %{channel_pid: channel_pid, peer_id: peer_id})
  end

  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  @impl true
  def init(%{channel_pid: channel_pid, peer_id: peer_id}) do
    Membrane.Logger.info("Spawning room process: #{inspect(self())}")

    rtc_engine_options = [
      id: UUID.uuid4(),
      display_manager?: false
    ]

    {:ok, rtc_engine} = Membrane.RTC.Engine.start(rtc_engine_options, [])

    Engine.register(rtc_engine, self())
    Process.monitor(rtc_engine)
    Process.monitor(channel_pid)

    hls_endpoint = hls_endpoint(rtc_engine)
    webrtc_endpoint = webrtc_endpoint(rtc_engine, peer_id)

    :ok = Engine.add_endpoint(rtc_engine, hls_endpoint)
    :ok = Engine.add_endpoint(rtc_engine, webrtc_endpoint, id: peer_id)

    {:ok,
     %{
       rtc_engine: rtc_engine,
       channel_pid: channel_pid,
       peer_id: peer_id
     }}
  end

  @impl true
  def handle_info({:playlist_playable, :audio, _playlist_idl}, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info({:playlist_playable, :video, playlist_idl}, state) do
    send(state.channel_pid, {:playlist_playable, playlist_idl})
    {:noreply, state}
  end

  @impl true
  def handle_info(%EndpointMessage{endpoint_id: _to, message: {:media_event, data}}, state) do
    send(state.channel_pid, {:media_event, data})
    {:noreply, state}
  end

  @impl true
  def handle_info(
        %EndpointRemoved{endpoint_id: endpoint_id, endpoint_type: type},
        state
      ) do
    if type == WebRTC,
      do: Membrane.Logger.info("Peer #{inspect(endpoint_id)} left RTC Engine"),
      else:
        Membrane.Logger.info(
          "HLS Endpoint #{inspect(endpoint_id)} has been removed from RTC Engine"
        )

    {:noreply, state}
  end

  @impl true
  def handle_info(%EndpointCrashed{endpoint_id: endpoint_id}, state) do
    Membrane.Logger.error("Endpoint #{inspect(endpoint_id)} has crashed!")
    {:noreply, state}
  end

  @impl true
  def handle_info({:media_event, _from, event}, state) do
    Engine.message_endpoint(state.rtc_engine, state.peer_id, {:media_event, event})
    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    if pid == state.channel_pid, do: Engine.terminate(state.rtc_engine)
    {:stop, :normal, state}
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp hls_endpoint(rtc_engine) do
    %HLS{
      rtc_engine: rtc_engine,
      owner: self(),
      mixer_config: %MixerConfig{},
      output_directory:
        Application.fetch_env!(:membrane_webrtc_to_hls_demo, :hls_output_mount_path),
      hls_config: %HLSConfig{cleanup_after: Membrane.Time.second()}
    }
  end

  defp webrtc_endpoint(rtc_engine, peer_id) do
    turn_mock_ip = Application.fetch_env!(:membrane_webrtc_to_hls_demo, :integrated_turn_ip)
    turn_ip = if @mix_env == :prod, do: {0, 0, 0, 0}, else: turn_mock_ip

    turn_cert_file =
      case Application.fetch_env(:membrane_webrtc_to_hls_demo, :integrated_turn_cert_pkey) do
        {:ok, val} -> val
        :error -> nil
      end

    integrated_turn_options = [
      ip: turn_ip,
      mock_ip: turn_mock_ip,
      ports_range:
        Application.fetch_env!(:membrane_webrtc_to_hls_demo, :integrated_turn_port_range),
      cert_file: turn_cert_file
    ]

    network_options = [
      integrated_turn_options: integrated_turn_options,
      integrated_turn_domain:
        Application.fetch_env!(:membrane_webrtc_to_hls_demo, :integrated_turn_domain),
      dtls_pkey: Application.get_env(:membrane_webrtc_to_hls_demo, :dtls_pkey),
      dtls_cert: Application.get_env(:membrane_webrtc_to_hls_demo, :dtls_cert)
    ]

    tcp_turn_port = Application.get_env(:membrane_webrtc_to_hls_demo, :integrated_tcp_turn_port)
    TURNManager.ensure_tcp_turn_launched(integrated_turn_options, port: tcp_turn_port)

    if turn_cert_file do
      tls_turn_port = Application.get_env(:membrane_webrtc_to_hls_demo, :integrated_tls_turn_port)
      TURNManager.ensure_tls_turn_launched(integrated_turn_options, port: tls_turn_port)
    end

    handshake_opts =
      if network_options[:dtls_pkey] &&
           network_options[:dtls_cert] do
        [
          client_mode: false,
          dtls_srtp: true,
          pkey: network_options[:dtls_pkey],
          cert: network_options[:dtls_cert]
        ]
      else
        [
          client_mode: false,
          dtls_srtp: true
        ]
      end

    %WebRTC{
      rtc_engine: rtc_engine,
      ice_name: peer_id,
      owner: self(),
      integrated_turn_options: network_options[:integrated_turn_options],
      integrated_turn_domain: network_options[:integrated_turn_domain],
      handshake_opts: handshake_opts,
      log_metadata: [peer_id: peer_id],
      webrtc_extensions: [Mid, Rid],
      filter_codecs: &filter_codecs_h264/1
    }
  end

  defp filter_codecs_h264(%Encoding{name: "H264", format_params: fmtp}) do
    import Bitwise

    # Only accept constrained baseline
    # based on RFC 6184, Table 5.
    case fmtp.profile_level_id >>> 16 do
      0x42 -> (fmtp.profile_level_id &&& 0x00_4F_00) == 0x00_40_00
      0x4D -> (fmtp.profile_level_id &&& 0x00_8F_00) == 0x00_80_00
      0x58 -> (fmtp.profile_level_id &&& 0x00_CF_00) == 0x00_C0_00
      _otherwise -> false
    end
  end

  defp filter_codecs_h264(encoding), do: filter_codecs(encoding)
  defp filter_codecs(%Encoding{name: "opus"}), do: true
  defp filter_codecs(_rtp_mapping), do: false
end
