defmodule Membrane.AudioFiller do
  @moduledoc """
  Element ensures that audio will be consistent by filling 'holes' with silence.
  In order for AudioFiller to work properly, all buffers processed have to have correct timestamps (pts).
  """

  use Membrane.Filter
  alias Membrane.Buffer
  alias Membrane.RawAudio

  def_options(
    min_audio_loss: [
      spec: pos_integer(),
      default: 10_000,
      description: """
      Minimal time of audio loss (in nanoseconds) that filler should fill with silence
      """
    ]
  )

  def_input_pad(:input,
    caps: RawAudio,
    demand_mode: :auto
  )

  def_output_pad(:output,
    caps: RawAudio,
    demand_mode: :auto
  )

  @impl true
  def handle_init(%__MODULE__{} = options) do
    state =
      options
      |> Map.from_struct()
      |> Map.merge(%{
        last_pts: nil,
        last_payload: nil,
        lost_audio_duration: 0,
        caps: nil
      })

    {:ok, state}
  end

  @impl true
  def handle_caps(:input, %RawAudio{} = caps, _context, state),
    do: {{:ok, caps: {:output, caps}}, %{state | caps: caps}}

  @impl true
  def handle_process(:input, buffer, _ctx, %{last_pts: nil} = state),
    do:
      {{:ok, [buffer: {:output, buffer}]},
       %{state | last_pts: buffer.pts, last_payload: byte_size(buffer.payload)}}

  @impl true
  def handle_process(:input, buffer, _ctx, state) do
    pts_duration = buffer.pts - state.last_pts
    last_payload_duration = RawAudio.bytes_to_time(state.last_payload, state.caps)
    lost_audio_duration = state.lost_audio_duration + (pts_duration - last_payload_duration)

    {state, buffers} =
      if lost_audio_duration > state.min_audio_loss do
        new_pts = state.last_pts + last_payload_duration

        {%{state | lost_audio_duration: 0},
         [
           %Buffer{pts: new_pts, payload: RawAudio.silence(state.caps, lost_audio_duration)},
           buffer
         ]}
      else
        {%{state | lost_audio_duration: 0}, [buffer]}
      end

    {{:ok, [buffer: {:output, buffers}]},
     %{state | last_pts: buffer.pts, last_payload: byte_size(buffer.payload)}}
  end
end
