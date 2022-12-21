defmodule Membrane.RTC.Engine.Endpoint.HLS.MobileLayoutMaker do
  @moduledoc """
  Module representing function for updating video layout for the HLS stream.
  """
  @behaviour Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker

  require Membrane.Pad

  alias Membrane.Pad
  alias Membrane.VideoCompositor.RustStructs.RawVideo
  alias Membrane.VideoCompositor.RustStructs.VideoPlacement

  @impl true
  def init(output_caps), do: %{tracks: %{}, output_caps: output_caps}

  @impl true
  def track_added(state, %{metadata: %{"mainPresenter" => true}} = track) do
    new_state = put_in(state, [:tracks, track.id], track)
    updated_layout = [{Pad.ref(:input, track.id), get_track_layout(:main, 0, state.output_caps)}]

    {updated_layout, new_state}
  end

  @impl true
  def track_added(state, track) do
    new_state = put_in(state, [:tracks, track.id], track)
    {update_layout(new_state), new_state}
  end

  @impl true
  def track_removed(state, track) do
    {_, new_state} = pop_in(state, [:tracks, track.id])
    {update_layout(new_state), new_state}
  end

  @spec update_layout(%{tracks: map(), output_caps: RawVideo.t()}) ::
          list({Pad.ref_t(), VideoPlacement.t()})
  defp update_layout(%{tracks: tracks, output_caps: output_caps}) do
    tracks
    |> Enum.filter(fn {_id, track} ->
      track.type == :video and not track.metadata["mainPresenter"]
    end)
    |> Enum.with_index()
    |> Enum.flat_map(fn {{_id, track}, index} ->
      track_layout = get_track_layout(:basic, index, output_caps)
      blank_layout = get_track_layout(:blank, index, output_caps)

      [
        {Pad.ref(:input, track.id), track_layout},
        {Pad.ref(:input, {:blank, track.id}), blank_layout}
      ]
    end)
  end

  @spec get_track_layout(:basic | :blank | :main, integer(), %{
          :height => integer(),
          :width => integer(),
          optional(any()) => any()
        }) :: VideoPlacement.t()
  defp get_track_layout(:blank, index, %{width: width, height: height}) do
    position = {round(index / 2 * width), height - round(1 / 4 * height)}
    display_size = {round(1 / 2 * width), round(1 / 4 * height)}

    %VideoPlacement{
      position: position,
      display_size: display_size,
      z_value: 0.2
    }
  end

  defp get_track_layout(:main, _index, %{width: width, height: height}),
    do: %VideoPlacement{
      position: {0, 0},
      display_size: {width, height},
      z_value: 0.1
    }

  defp get_track_layout(:basic, index, %{width: width, height: height}) do
    position = {round(index / 2 * width) + 5, height - round(1 / 4 * height) + 5}
    display_size = {round(1 / 2 * width) - 10, round(1 / 4 * height) - 10}

    %VideoPlacement{
      position: position,
      display_size: display_size,
      z_value: 0.3
    }
  end
end
