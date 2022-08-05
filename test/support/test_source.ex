defmodule Membrane.RTC.Engine.Support.TestSource do
  @moduledoc false

  use Membrane.Source
  use Bitwise

  alias Membrane.{Buffer, Time}

  def_options interval: [
                spec: Time.t()
              ],
              payload: [
                spec: binary()
              ]

  def_output_pad :output,
    availability: :always,
    caps: :any

  @impl true
  def handle_init(%__MODULE__{} = opts) do
    {:ok, opts |> Map.from_struct() |> Map.merge(%{seq_num: 0, active?: true})}
  end

  @impl true
  def handle_prepared_to_playing(_ctx, state) do
    {{:ok, caps: {:output, %Membrane.RTP{}}}, state}
  end

  @impl true
  def handle_demand(:output, _size, :buffers, _ctx, state) do
    send(self(), :supply_demand)
    {:ok, state}
  end

  @impl true
  def handle_other(:supply_demand, %{playback_state: :playing} = _ctx, %{active?: true} = state) do
    timestamp = state.seq_num * state.interval

    buffer = %Buffer{
      payload: state.payload,
      metadata: %{
        rtp: %{
          sequence_number: rem(state.seq_num, 1 <<< 16),
          timestamp: Time.as_milliseconds(timestamp)
        }
      },
      pts: timestamp,
      dts: timestamp
    }

    state = Map.update!(state, :seq_num, &(&1 + 1))
    {{:ok, buffer: {:output, buffer}, redemand: :output}, state}
  end

  @impl true
  def handle_other(:supply_demand, _ctx, state), do: {:ok, state}

  @impl true
  def handle_other({:set_active, active?}, _ctx, state),
    do: {{:ok, redemand: :output}, %{state | active?: active?}}

  @impl true
  def handle_event(:output, event, _ctx, state) do
    {{:ok, notify: %Membrane.Testing.Notification{payload: {:event, event}}}, state}
  end
end
