defmodule Membrane.RTC.Engine.Endpoint.Validator do
  @moduledoc false

  use Membrane.Filter

  require Logger

  alias Membrane.Buffer

  def_input_pad :input, accepted_format: Membrane.H264

  def_output_pad :output, accepted_format: Membrane.H264

  @impl true
  def handle_init(_ctx, _options) do
    {[], %{stream_format?: false, keyframe_processing?: false}}
  end

  @impl true
  def handle_stream_format(_pad, stream_format, _ctx, _state) do
    {[stream_format: {:output, stream_format}], %{stream_format?: true, keyframe_processing?: false}}
  end

  @impl true
  def handle_buffer(_pad, %Buffer{} = buffer, _ctx, %{stream_format?: true}) do
    type = buffer.metadata.h264.type

    cond do
      type in [:sps, :pps] ->
        {[buffer: {:output, buffer}], %{stream_format?: false, keyframe_processing?: true}}

      type == :idr ->
        Logger.error("Got idr frame without sps and pps after new stream format: #{inspect(buffer)}")
        {[buffer: {:output, buffer}], %{stream_format?: false, keyframe_processing?: false}}

      true ->
        Logger.error("Got non idr frame just after new stream format: #{inspect(buffer)}")
        {[buffer: {:output, buffer}], %{stream_format?: false, keyframe_processing?: false}}
    end
  end


  @impl true
  def handle_buffer(_pad, %Buffer{} = buffer, _ctx, %{keyframe_processing?: true}) do
    type = buffer.metadata.h264.type

    cond do
      type in [:sps, :pps] ->
        {[buffer: {:output, buffer}], %{keyframe_processing?: true}}

      type == :idr ->
        {[buffer: {:output, buffer}], %{keyframe_processing?: false}}

      true ->
        Logger.error("Got non idr frame after sps and pps and new stream format: #{inspect(buffer)}")
        {[buffer: {:output, buffer}], %{keyframe_processing?: false}}
    end
  end

  @impl true
  def handle_buffer(_pad, buffer, _ctx, state) do
    {[buffer: {:output, buffer}], state}
  end
end
