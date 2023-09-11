defmodule Membrane.File.Sink do
  @moduledoc """
  Element that creates a file and stores incoming buffers there (in binary format).

  When `Membrane.File.SeekEvent` is received, the element starts writing buffers starting
  from `position`. By default, it overwrites previously stored bytes. You can set `insert?`
  field of the event to `true` to start inserting new buffers without overwriting previous ones.
  Please note, that inserting requires rewriting the file, what negatively impacts performance.
  For more information refer to `Membrane.File.SeekEvent` moduledoc.
  """
  use Membrane.Sink

  alias Membrane.File.SeekEvent

  @common_file Membrane.File.CommonFileBehaviour.get_impl()

  def_options location: [
                spec: Path.t(),
                description: "Path of the output file"
              ]

  def_input_pad :input, demand_unit: :buffers, accepted_format: _any

  @impl true
  def handle_init(_ctx, %__MODULE__{location: location}) do
    {[],
     %{
       location: Path.expand(location),
       temp_location: Path.expand(location <> ".tmp"),
       fd: nil,
       temp_fd: nil
     }}
  end

  @impl true
  def handle_setup(_ctx, %{location: location} = state) do
    fd = @common_file.open!(location, [:read, :write])
    :ok = @common_file.truncate!(fd)

    {[], %{state | fd: fd}}
  end

  @impl true
  def handle_playing(_ctx, state) do
    {[demand: :input], state}
  end

  @impl true
  def handle_write(:input, buffer, _ctx, %{fd: fd} = state) do
    :ok = @common_file.write!(fd, buffer)
    {[demand: :input], state}
  end

  @impl true
  def handle_event(:input, %SeekEvent{insert?: insert?, position: position}, _ctx, state) do
    state =
      if insert?,
        do: split_file(state, position),
        else: seek_file(state, position)

    {[], state}
  end

  def handle_event(pad, event, ctx, state), do: super(pad, event, ctx, state)

  @impl true
  def handle_terminate_request(_ctx, state) do
    state = maybe_merge_temporary(state)
    @common_file.close!(state.fd)

    {[terminate: :normal], %{state | fd: nil}}
  end

  defp seek_file(%{fd: fd} = state, position) do
    state = maybe_merge_temporary(state)
    _position = @common_file.seek!(fd, position)
    state
  end

  defp split_file(%{fd: fd} = state, position) do
    state =
      state
      |> seek_file(position)
      |> open_temporary()

    :ok = @common_file.split!(fd, state.temp_fd)
    state
  end

  defp maybe_merge_temporary(%{temp_fd: nil} = state), do: state

  defp maybe_merge_temporary(%{fd: fd, temp_fd: temp_fd, temp_location: temp_location} = state) do
    # TODO: Consider improving performance for multi-insertion scenarios by using
    # multiple temporary files and merging them only once on `handle_terminate_request/2`.
    copy_and_remove_temporary(fd, temp_fd, temp_location)
    %{state | temp_fd: nil}
  end

  defp open_temporary(%{temp_fd: nil, temp_location: temp_location} = state) do
    temp_fd = @common_file.open!(temp_location, [:read, :exclusive])

    %{state | temp_fd: temp_fd}
  end

  defp copy_and_remove_temporary(fd, temp_fd, temp_location) do
    _bytes_copied = @common_file.copy!(temp_fd, fd)
    :ok = @common_file.close!(temp_fd)
    :ok = @common_file.rm!(temp_location)
  end
end
