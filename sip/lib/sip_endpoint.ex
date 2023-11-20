defmodule Membrane.RTC.Engine.Endpoint.SIP do
  @moduledoc """
  An Endpoint responsible for connecting to a remote RTSP stream source
  and sending the appropriate media track to other Endpoints.

  ## Limitations
  Currently, only H264 streams are supported.

  If the RTSP Endpoint has yet to successfully connect to the stream source,
  a reconnect attempt can be made, either by the endpoint itself,
  or by calling `#{inspect(__MODULE__)}.request_reconnect/2`.
  Once connected, however, if RTSP signalling crashes, the endpoint WILL NOT be able
  to reconnect, and will shut down instead.
  A manual restart of the endpoint will then be needed to restore its functionality.

  Note that the RTSP Endpoint is not guaranteed to work when NAT is used
  by either side of the connection. By default, the endpoint will attempt to
  create client-side NAT binding by sending an empty datagram from client to source,
  after the completion of RTSP setup. This behaviour may be disabled by setting
  the option `:pierce_nat` to `false`.
  """

  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.SIP.Client
  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender
  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
  alias Membrane.RTC.Engine.Track
  alias Membrane.Time

  @doc """
  Request that a given RTSP Endpoint attempts a reconnect to the remote stream source.
  """
  # @spec request_reconnect(rtc_engine :: pid(), endpoint_id :: String.t()) :: :ok
  # def request_reconnect(rtc_engine, endpoint_id) do
  #   Engine.message_endpoint(rtc_engine, endpoint_id, :reconnect)
  # end

  # @rtp_port 20_000
  # @max_reconnect_attempts 3
  # @reconnect_delay 15_000
  # @keep_alive_interval 15_000

  # Increased buffer size helps ensure the stability of transmission and eliminate video artefacts
  # @recv_buffer_size 1024 * 1024

  defmodule RegistrarCredentials do
    @moduledoc """
    Module describing credentials needed to connect with SIP registrar server
    """

    @typedoc """
    Describes SIP registrar credentials structure.

    * `domain` - domain of the registrar server
    * `username` - your username in registrar service
    * `password` - your password in registrar service
    """
    @type t() :: %__MODULE__{
            domain: URI.t(),
            username: binary(),
            password: binary()
          }

    @enforce_keys [:domain, :username, :password]
    defstruct @enforce_keys
  end

  def_output_pad :output,
    demand_unit: :buffers,
    accepted_format: Membrane.RTP,
    availability: :on_request

  def_input_pad :input,
    demand_unit: :buffers,
    accepted_format: Membrane.RTP,
    availability: :on_request

  def_options rtc_engine: [
                spec: pid(),
                description: "PID of parent Engine"
              ],
              registrar_credentials: [
                spec: RegistrarCredentials.t(),
                description: "Credentials needed to connect with SIP registrar server"
              ],
              rtp_port: [
                spec: 1..65_535,
                description: "Local udp port RTP stream will be received at",
              ],
              sip_port: [
                spec: 1..65_535,
                description: "Local udp port SIP messages will be received at",
              ]

  @impl true
  def handle_init(_ctx, opts) do
    state = %{
      rtc_engine: opts.rtc_engine,
      registrar_credentials: opts.registrar_credentials,
      rtp_port: opts.rtp_port,
      sip_port: opts.sip_port,
      outgoing_tracks: [],
      incoming_track: nil,
      outgoing_ssrc: nil,
      incoming_ssrc: nil,
      sip_client: nil
    }

    client_opts = [
      registrar_credentials: state.registrar_credentials,
      rtp_port: state.rtp_port,
      sip_port: state.sip_port,
      endpoint: self()
    ]

    {:ok, pid} = Client.start_link(client_opts)

    {[], %{state | sip_client: pid}}
  end

  @impl true
  def handle_parent_notification({:new_tracks, tracks}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    state =
      Enum.filter(tracks, fn track -> track.type == :audio && track.origin != endpoint_id end)
      |> Enum.reduce(state, fn track, state ->
        case Engine.subscribe(state.rtc_engine, endpoint_id, track.id) do
          :ok ->
            put_in(state, [:tracks, track.id], track)

          {:error, :invalid_track_id} ->
            Membrane.Logger.info("""
            Couldn't subscribe to the track: #{inspect(track.id)}. No such track.
            It had to be removed just after publishing it. Ignoring.
            """)

            state

          {:error, reason} ->
            raise "Couldn't subscribe to the track: #{inspect(track.id)}. Reason: #{inspect(reason)}"
        end
      end)

    {[], state}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:input, track_id) = pad,
        ctx,
        state
      ) do

    track = Map.get(state.outgoing_tracks, track_id)
    spec = [
      bin_input(pad)
      |> child({:track_receiver, track.id}, %TrackReceiver{
        track: track,
        initial_target_variant: :high
      })
      |> child({:depayloader, track.id}, Track.get_depayloader(track))
      |> child({:opus_decoder, track.id}, Membrane.Opus.Decoder)
      |> via_in(Pad.ref(:input, {:extra, track.id}))
      |> get_child(:audio_mixer)
    ]
    actions = [
      spec: spec
    ]

    {actions, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, {track_id, :high}) = pad, _ctx, state)
      when track_id == state.incoming_track.id do
    Membrane.Logger.debug("Pad added for track #{inspect(track_id)}, variant :high")

    structure = [
      get_child(:rtp)
      |> via_out(Pad.ref(:output, state.incoming_ssrc),
          options: [depayloader: Membrane.RTP.G711.Depayloader]
        )
      |> child(:g711_decoder, Membrane.G711.FFmpeg.Decoder)
      |> child(:converter, %Membrane.FFmpeg.SWResample.Converter{
        input_stream_format: %RawAudio{channels: 1, sample_format: :s16le, sample_rate: 8_000},
        output_stream_format: %RawAudio{channels: 1, sample_format: :s16le, sample_rate: 48_000}
      })
      |> child(:opus_encoder, %Membrane.Opus.Encoder{
          input_stream_format: %Membrane.RawAudio{
            channels: 1,
            sample_rate: 48_000,
            sample_format: :s16le
          }
        })
      |> child(:payloader, %Membrane.RTP.PayloaderBin{
          payloader: Membrane.RTP.Opus.Payloader,
          ssrc: state.incoming_ssrc,
          payload_type: Membrane.RTP.PayloadFormat.get(:OPUS)
          clock_rate: 48_000
        })
      |> child(
          {:track_sender, track_id},
          %TrackSender{
            track: state.incoming_track,
            variant_bitrates: %{high: 50}
          }
        )
      |> via_out(pad)
      |> bin_output(pad)
    ]

    {[spec: structure], state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:output, {_track_id, _variant}), _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:remove_tracks, _tracks}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification(msg, _ctx, state) do
    Membrane.Logger.warning("Unexpected message: #{inspect(msg)}. Ignoring.")
    {[], state}
  end

  @impl true
  def handle_child_notification(
        {:new_rtp_stream, ssrc, fmt, _extensions} = msg,
        :rtp,
        _ctx,
        state
      )
      when is_nil(state.incoming_ssrc) or ssrc == state.incoming_ssrc do
    Membrane.Logger.debug("New RTP stream connected: #{inspect(msg)}")

    state = %{state | incoming_ssrc: ssrc}

    expected_fmt = state.incoming_track.ctx.rtpmap.payload_type

    if fmt != expected_fmt do
      raise("""
      Payload type mismatch between RTP mapping and received stream
      (expected #{inspect(expected_fmt)}, got #{inspect(fmt)})
      """)
    end

    {[
       notify_parent: {:forward_to_parent, :new_rtp_stream},
       notify_parent: {:track_ready, state.incoming_track.id, :high, state.incoming_track.encoding}
     ], state}
  end

  @impl true
  def handle_child_notification(
        {:new_rtp_stream, _ssrc, _fmt, _extensions} = msg,
        :rtp,
        _ctx,
        _state
      ) do
    raise("Received unexpected, second RTP stream: #{inspect(msg)}")
  end

  @impl true
  def handle_child_notification(notification, element, _ctx, state) do
    Membrane.Logger.warning(
      "Unexpected notification from `#{inspect(element)}`: #{inspect(notification)}. Ignoring."
    )
    {[], state}
  end

  @impl true
  def handle_info({:call_ready, options}, ctx, state) do
    Membrane.Logger.debug("Endpoint received source options: #{inspect(options)}")

    {payload_type, rtpmap} = options.rtpmap
    {:endpoint, endpoint_id} = ctx.name

    track =
      Track.new(
        :audio,
        Track.stream_id(),
        endpoint_id,
        rtpmap.encoding,
        rtpmap.clock_rate,
        %ExSDP.Attribute.FMTP{pt: payload_type}
      )

    Membrane.Logger.debug("Publishing new RTSP track: #{inspect(track)}")

    # pierce_nat_ctx =
    #   if state.pierce_nat, do: %{uri: state.source_uri, port: options.server_port}, else: nil

    structure = [
      child(:udp_source, %Membrane.UDP.Source{
        local_port_no: state.rtp_port,
        # pierce_nat_ctx: pierce_nat_ctx,
        # recv_buffer_size: @recv_buffer_size
      })
      |> via_in(Pad.ref(:rtp_input, make_ref()))
      |> child(:rtp, %Membrane.RTP.SessionBin{
        fmt_mapping: %{rtpmap.payload_type => {rtpmap.encoding, rtpmap.clock_rate}}
      }),
      child(:audio_mixer, %Membrane.LiveAudioMixer{
        latency: nil,
        stream_format: %Membrane.RawAudio{
          channels: 1,
          sample_rate: 48_000,
          sample_format: :s16le
        }
      })
      |> child(:converter, %Membrane.FFmpeg.SWResample.Converter{
        input_stream_format: %RawAudio{channels: 1, sample_format: :s16le, sample_rate: 48_000},
        output_stream_format: %RawAudio{channels: 1, sample_format: :s16le, sample_rate: 8_000}
      })
      |> child(:g711_encoder, ...)
      |> via_in(Pad.ref(:input, outgoing_ssrc), options: [payloader: G711...])
      |> get_child(:rtp)
      |> via_out(Pad.ref(:rtp_output, outgoing_ssrc), options: [encoding: :PCMA])
      |> child(:video_sink, %UDP.Sink{
        destination_port_no: _______,
        destination_address: {127, 0, 0, 1}
      })



      |> via_in(Pad.ref(:input, make_ref())
      |> child(:rtp, %Membrane.RTP.SessionBin
    ]

    actions = [
      spec: structure,
      notify_parent: :ready,
      notify_child: {:audio_mixer, {:start_mixing, Time.milliseconds(200)}},
      notify_parent: {:forward_to_parent, :call_ready},
      notify_parent: {:publish, {:new_tracks, [track]}}
    ]

    state = %{state | incoming_track: track}

    {actions, state}
  end

  @impl true
  def handle_info({:sip_info, {:call_end, reason} = msg}, _ctx, state) do
    Membrane.Logger.warning("SIP Endpoint: Call ended with reason: #{inspect(reason)}")
    {[notify_parent: {:forward_to_parent, msg}, terminate: :shutdown], state}
  end

  @impl true
  def handle_terminate_request(ctx, state) do
    Process.send_after(self(), :terminate, @terminate_timeout)

    actions =
      if Map.has_key?(ctx.children, :audio_mixer) do
        [
          notify_child: {:audio_mixer, :schedule_eos},
          notify_child: {:compositor, :schedule_eos}
        ]
      else
        []
      end

    children_to_remove =
      state.tracks
      |> Enum.flat_map(fn {id, _track} -> Enum.map(@track_children, &{&1, id}) end)
      |> Enum.filter(&Map.has_key?(ctx.children, &1))

    actions = actions ++ [remove_child: children_to_remove]
    {actions, %{state | terminating?: true}}
  end

  @impl true
  def handle_info(:terminate, _ctx, %{terminating?: true} = state),
    do: {[terminate: :normal], state}

  # @impl true
  # def handle_info({:sip_info, :max_reconnects}, _ctx, state) do
  #   Membrane.Logger.warning("RTSP Endpoint: Max reconnect attempts reached.")
  #   {[notify_parent: {:forward_to_parent, :max_reconnects}], state}
  # end

  # @impl true
  # def handle_info({:sip_info, :disconnected}, _ctx, state) do
  #   Membrane.Logger.error("""
  #   RTSP Endpoint disconnected from source.
  #   The endpoint is now functionally useless, it will not be able to reconnect and is thus shutting down
  #   """)

  #   {[notify_parent: {:forward_to_parent, :disconnected}, terminate: :shutdown], state}
  # end

  @impl true
  def handle_info(info, _ctx, state) do
    Membrane.Logger.warning("Unexpected info: #{inspect(info)}. Ignoring.")
    {[], state}
  end

end
