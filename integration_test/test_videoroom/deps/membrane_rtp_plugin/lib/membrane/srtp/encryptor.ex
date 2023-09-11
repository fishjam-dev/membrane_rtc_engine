if Code.ensure_loaded?(ExLibSRTP) do
  defmodule Membrane.SRTP.Encryptor do
    @moduledoc """
    Converts plain RTP packets to SRTP.

    Requires adding [srtp](https://github.com/membraneframework/elixir_libsrtp) dependency to work.
    """
    use Membrane.Filter

    require Membrane.Logger

    alias Membrane.{Buffer, RTP, SRTP}

    def_input_pad :input, accepted_format: _any, demand_mode: :auto
    def_output_pad :output, accepted_format: _any, demand_mode: :auto

    defguardp is_protection_error_fatal(type, reason)
              when type == :rtcp or
                     (type == :rtp and reason not in [:replay_fail, :replay_old])

    def_options policies: [
                  spec: [ExLibSRTP.Policy.t()],
                  default: [],
                  description: """
                  List of SRTP policies to use for encrypting packets.
                  See `t:ExLibSRTP.Policy.t/0` for details.
                  """
                ]

    @impl true
    def handle_init(_ctx, %__MODULE__{policies: policies}) do
      state = %{
        policies: policies,
        srtp: nil,
        queue: []
      }

      {[], state}
    end

    @impl true
    def handle_setup(_ctx, state) do
      srtp = ExLibSRTP.new()

      state.policies
      |> Bunch.listify()
      |> Enum.each(&ExLibSRTP.add_stream(srtp, &1))

      {[], %{state | srtp: srtp}}
    end

    @impl true
    def handle_start_of_stream(:input, _ctx, state) do
      if state.policies == [] do
        # TODO: remove when dynamic switching between automatic and manual demands will be supported
        {[start_timer: {:policy_timer, Membrane.Time.seconds(5)}], state}
      else
        {[], state}
      end
    end

    @impl true
    def handle_tick(:policy_timer, ctx, state) do
      if state.policies != [] or ctx.pads.input.end_of_stream? do
        {[stop_timer: :policy_timer], state}
      else
        raise "No SRTP policies arrived in 5 seconds"
      end
    end

    @impl true
    def handle_event(_pad, %SRTP.KeyingMaterialEvent{} = event, _ctx, %{policies: []} = state) do
      {:ok, crypto_profile} =
        ExLibSRTP.Policy.crypto_profile_from_dtls_srtp_protection_profile(
          event.protection_profile
        )

      policy = %ExLibSRTP.Policy{
        ssrc: :any_outbound,
        key: event.local_keying_material,
        rtp: crypto_profile,
        rtcp: crypto_profile
      }

      :ok = ExLibSRTP.add_stream(state.srtp, policy)
      buffers = state.queue |> Enum.reverse() |> Enum.flat_map(&protect_buffer(&1, state.srtp))
      {[buffer: {:output, buffers}], %{Map.put(state, :policies, [policy]) | queue: []}}
    end

    @impl true
    def handle_event(_pad, %SRTP.KeyingMaterialEvent{}, _ctx, state) do
      Membrane.Logger.warn("Got unexpected SRTP.KeyingMaterialEvent. Ignoring.")
      {[], state}
    end

    @impl true
    def handle_event(pad, other, ctx, state), do: super(pad, other, ctx, state)

    @impl true
    def handle_process(:input, buffer, _ctx, %{policies: []} = state) do
      {[], Map.update!(state, :queue, &[buffer | &1])}
    end

    @impl true
    def handle_process(:input, buffer, _ctx, state) do
      {[buffer: {:output, protect_buffer(buffer, state.srtp)}], state}
    end

    defp protect_buffer(buffer, srtp) do
      %Buffer{payload: payload} = buffer
      packet_type = RTP.Packet.identify(payload)

      protection_result =
        case packet_type do
          :rtp -> ExLibSRTP.protect(srtp, payload)
          :rtcp -> ExLibSRTP.protect_rtcp(srtp, payload)
        end

      case protection_result do
        {:ok, payload} ->
          [%Buffer{buffer | payload: payload}]

        {:error, reason} when is_protection_error_fatal(packet_type, reason) ->
          raise "Failed to protect #{inspect(packet_type)} due to unhandled error #{reason}"

        {:error, reason} ->
          Membrane.Logger.warn("Ignoring #{inspect(packet_type)} packet due to `#{reason}`")
          []
      end
    end
  end
end
