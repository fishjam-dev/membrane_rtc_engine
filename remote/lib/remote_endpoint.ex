defmodule Membrane.RTC.Engine.Endpoint.Remote do
  @moduledoc """
  A Remote Endpoint
  """

  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Track
  alias Membrane.RTC.Engine.Endpoint.Remote
  alias Membrane.Time

  def_input_pad :input,
    demand_unit: :buffers,
    accepted_format: _any,
    availability: :on_request

  def_output_pad :output,
    demand_unit: :buffers,
    accepted_format: _any,
    availability: :on_request

  def_input_pad :remote_input,
    demand_unit: :buffers,
    accepted_format: _any,
    availability: :on_request

  def_output_pad :remote_output,
    demand_unit: :buffers,
    accepted_format: _any,
    availability: :on_request

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              owner: [
                spec: pid(),
                description: """
                Pid of parent all notifications will be send to.
                These notifications are:
                * `{:playlist_playable, content_type}`
                * `{:cleanup, clean_function}`
                """
              ],
              # rtp_config: [
              #   spec: RTPConfig.t() | nil,
              #   default: nil,
              #   description: """
              #   Config of RTP connection between Remote Endpoints
              #   """
              # ],
              connection_setup: [
                spec: Remote.ConnectionSetup.t() | nil,
                default: nil
              ]

  @impl true
  def handle_setup(ctx, state) do
    IO.inspect(state, label: "setup")
    {:endpoint, endpoint_id} = ctx.name
    myself = :"#{endpoint_id}"
    Process.register(self(), myself)

    {actions, state_update} = case state.connection_setup do
      %Remote.ConnectionSetup{token: token, link_to: link_to} ->
        send(link_to, {:handshake, %Remote.ConnectionSetup{token: token, link_to: {myself, Node.self()}}, token})
        {
          [setup: :incomplete],
          %{token: token, partner_endpoint: link_to, status: :send_link_proposal, myself: myself}
        }
      nil ->
        token = :crypto.strong_rand_bytes(32) |> Base.url_encode64(padding: false)
        setup = %Remote.ConnectionSetup{
          token: token,
          link_to: {myself, Node.self()}
        }
        {
          [setup: :incomplete, notify_parent: {:forward_to_parent, {:remote_endpoint_created, setup}}],
          %{token: token, partner_endpoint: nil, status: :waiting_for_partner, myself: myself}
        }
    end
    {actions, Map.merge(state, state_update)}
  end

  def handle_info({:handshake, %Remote.ConnectionSetup{token: new_token, link_to: link_to}, incoming_token}, context, %{token: token, status: status, partner_endpoint: partner_endpoint} = state) when token == incoming_token do
    IO.inspect(link_to || partner_endpoint, label: "handshake")
    case status do
      :handshaked ->
        {[], state}
      _other ->
        send(link_to || partner_endpoint, {:handshake, %Remote.ConnectionSetup{}, token})
        {
          [notify_parent: :ready, setup: :complete],
          %{state | partner_endpoint: link_to, status: :handshaked}
        }
    end
  end

  def handle_info({:handshake, _partner_endpoint, incoming_token}, _context, %{token: token} = state) do
    {[], state}
  end







  #   state =
  #     options
  #     |> Map.from_struct()
  #     |> Map.merge(%{
  #       connection_setup: %Remote.ConnectionSetup{
  #         token; :crypto.
  #       }
  #     })
  #   {[notify_parent: {:forward_to_parent, {:remote_endpoint_created, stream}}], state}
  # end


  # def handle_pad_added(Pad.ref(:input, track_id) = pad,
  #   ctx,
  #   state) do

  # h264_spec = [
  #   bin_input(pad)
  #   |> via_in(Pad.ref(:input, track_id), options: [payloader: RTP.H264.Payloader])
  #   |> child(:rtp, %RTP.SessionBin{
  #     secure?: secure?,
  #     srtp_policies: [
  #       %ExLibSRTP.Policy{
  #         ssrc: :any_inbound,
  #         key: srtp_key
  #       }
  #     ]
  #   })
  #   |> via_out(Pad.ref(:rtp_output, track_id), options: [encoding: :H264])
  #   |> child(:video_sink, %UDP.Sink{
  #     destination_port_no: video_port,
  #     destination_address: {127, 0, 0, 1}
  #   }),
  # ]

  # opus_spec = [
  #   bin_input(pad)
  #   |> via_in(Pad.ref(:input, track_id), options: [payloader: RTP.Opus.Payloader])
  #   |> get_child(:rtp)
  #   |> via_out(Pad.ref(:rtp_output, track_id), options: [encoding: :OPUS])
  #   |> child(:audio_sink, %UDP.Sink{
  #     destination_port_no: audio_port,
  #     destination_address: {127, 0, 0, 1}
  #   })

  # ]

  @impl true
  def handle_parent_notification({:ready, _other_endpoints}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:new_endpoint, _endpoint}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:endpoint_removed, _endpoint_id}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:new_tracks, tracks}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:remove_tracks, _list}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification(:start, _ctx, state) do
    track_ready = {:track_ready, state.track.id, :high, state.track.encoding}
    {[notify_parent: track_ready], state}
  end

  @typedoc """
  Type of messages that need to be handled by each endpoint.
  """
  # @type published_message_t() ::
  #         {:new_tracks, [Track.t()]}
  #         | {:removed_tracks, [Track.t()]}
  #         | {:new_endpoint, Endpoint.t()}
  #         | {:endpoint_removed, Endpoint.id()}
  #         | {:track_metadata_updated, Track.t()}
  #         | {:endpoint_metadata_updated, Endpoint.t()}
  #         | {:tracks_priority, tracks :: list()}
  #         | ready_ack_msg_t()
  #         | TrackNotification.t()


end
