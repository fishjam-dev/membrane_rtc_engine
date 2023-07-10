if Code.ensure_loaded?(Membrane.VideoCompositor) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.CompositorHandler do
    @moduledoc """
    Module implementing `Membrane.VideoCompositor.Handler` behavoir.
    Responsible for updating `Membrane.VideoCompositor.Scene` for video compositor.
    This module handles layout up to three video tracks.

    ```ascii
    1) One presenter
         _ _ _ _ _ _ _ _ _
        |                 |
        |                 |
        |                 |
        |                 |
        |                 |
         - - - - - - - - -
    2) Two presenters
         _ _ _ _ _ _ _ _ _
        |        |        |
        |        |        |
        |        |        |
        |        |        |
        |        |        |
         - - - - - - - - -
    3) Three presenters
         _ _ _ _ _ _ _ _ _
        |        |        |
        |        |        |
        | _ _ _ _|_ _ _ _ |
        |    |       |    |
        |    |       |    |
         - - - - - - - - -
    ```
    """

    @behaviour Membrane.VideoCompositor.Handler

    alias Membrane.VideoCompositor.{VideoConfig, Scene}
    alias Membrane.RTC.Engine.Endpoints.HLS.CompositorHandler.Utils

    @impl true
    def handle_init(ctx) do
      %{output_stream_format: ctx.output_stream_format, screenShare?: false}
    end

    @impl true
    def handle_info(_msg, _ctx, _state) do
      raise "Not implemented"
    end

    @impl true
    def handle_inputs_change(
          inputs,
          _ctx,
          state
        ) do
      inputs_number = Enum.count(inputs)

      scene =
        inputs
        |> Enum.with_index()
        |> Enum.map(fn {{ref, %{stream_format: stream_format}}, index} ->
          video_config = get_video_config(stream_format, inputs_number, index, state)

          {ref, video_config}
        end)
        |> Enum.into(%{})
        |> then(fn video_configs -> %Scene{video_configs: video_configs} end)

      {scene, state}
    end

    defp get_video_config(input_stream_format, inputs_number, index, %{
           output_stream_format: output_stream_format
         }) do
      desired_stream_format = Utils.get_desired_stream_format(inputs_number, output_stream_format)

      placement =
        Utils.get_placement(
          desired_stream_format,
          inputs_number,
          index,
          input_stream_format,
          output_stream_format
        )

      scaled_stream_format = placement.size

      # Compositor works in two steps.
      # First it scales and places video creating output in `scaled_stream_format`.
      # Then it does tranformations like cropping or cutting video.
      # In transformation step compositor works on `scaled_stream_format` from previous step.
      transformations = Utils.get_transformations(desired_stream_format, scaled_stream_format)

      %VideoConfig{
        placement: placement,
        transformations: transformations
      }
    end
  end
end
