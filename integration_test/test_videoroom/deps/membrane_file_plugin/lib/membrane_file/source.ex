defmodule Membrane.File.Source do
  @moduledoc """
  Element that reads chunks of data from given file and sends them as buffers
  through the output pad.
  """
  use Membrane.Source

  alias Membrane.{Buffer, RemoteStream}

  @common_file Membrane.File.CommonFileBehaviour.get_impl()

  def_options location: [
                spec: Path.t(),
                description: "Path to the file"
              ],
              chunk_size: [
                spec: pos_integer(),
                default: 2048,
                description: "Size of chunks being read"
              ]

  def_output_pad :output, accepted_format: %RemoteStream{type: :bytestream}

  @impl true
  def handle_init(_ctx, %__MODULE__{location: location, chunk_size: size}) do
    {[],
     %{
       location: Path.expand(location),
       chunk_size: size,
       fd: nil
     }}
  end

  @impl true
  def handle_setup(_ctx, %{location: location} = state) do
    fd = @common_file.open!(location, :read)

    {[], %{state | fd: fd}}
  end

  @impl true
  def handle_playing(_ctx, state) do
    {[stream_format: {:output, %RemoteStream{type: :bytestream}}], state}
  end

  @impl true
  def handle_demand(:output, _size, :buffers, _ctx, %{chunk_size: chunk_size} = state),
    do: supply_demand(chunk_size, [redemand: :output], state)

  def handle_demand(:output, size, :bytes, _ctx, state),
    do: supply_demand(size, [], state)

  @impl true
  def handle_terminate_request(_ctx, state) do
    @common_file.close!(state.fd)

    {[terminate: :normal], %{state | fd: nil}}
  end

  defp supply_demand(size, redemand, %{fd: fd} = state) do
    actions =
      case @common_file.binread!(fd, size) do
        <<payload::binary>> when byte_size(payload) == size ->
          [buffer: {:output, %Buffer{payload: payload}}] ++ redemand

        <<payload::binary>> when byte_size(payload) < size ->
          [buffer: {:output, %Buffer{payload: payload}}, end_of_stream: :output]

        :eof ->
          [end_of_stream: :output]
      end

    {actions, state}
  end
end
