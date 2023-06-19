defmodule Membrane.RTC.Engine.Support.HLSStorage do
  @moduledoc false

  @behaviour Membrane.HTTPAdaptiveStream.Storage

  alias Membrane.HTTPAdaptiveStream.Storages.FileStorage

  @enforce_keys [:file_storage, :pid, :should_stop?]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          file_storage: FileStorage.t(),
          pid: any(),
          should_stop?: (__MODULE__.t() -> boolean())
        }

  @impl true
  def init(%__MODULE__{} = config) do
    config
    |> Map.from_struct()
    |> Map.update!(:file_storage, fn file_storage -> FileStorage.init(file_storage) end)
    |> Map.put(:stopped?, false)
    |> Map.put(:segments, %{
      video_segments: 0,
      audio_segments: 0,
      muxed_segments: 0
    })
  end

  @impl true
  def store(_parent_id, _resource_name, _content, _metadata, _context, %{stopped?: true} = state),
    do: {:ok, state}

  @impl true
  def store(parent_id, resource_name, content, metadata, context, state) do
    {result, file_storage} =
      FileStorage.store(parent_id, resource_name, content, metadata, context, state.file_storage)

    state =
      if context.type == :manifest do
        content = String.split(content, "\n")

        video_segments = Enum.count(content, &String.match?(&1, ~r/^video.*\.m4s/))
        audio_segments = Enum.count(content, &String.match?(&1, ~r/^audio.*\.m4s/))
        muxed_segments = Enum.count(content, &String.match?(&1, ~r/^muxed.*\.m4s/))

        segments = %{
          video_segments: video_segments,
          audio_segments: audio_segments,
          muxed_segments: muxed_segments
        }

        send(
          state.pid,
          {context.type, segments}
        )

        update_segments(segments, state)
      else
        state
      end

    if context.type == :segment do
      send(state.pid, {context.type, resource_name})
    end

    {result, %{state | file_storage: file_storage}}
  end

  defp update_segments(
         %{
           video_segments: video_segments,
           audio_segments: audio_segments,
           muxed_segments: muxed_segments
         },
         state
       ) do
    audio_segments = max(audio_segments, state.segments.audio_segments)
    video_segments = max(video_segments, state.segments.video_segments)
    muxed_segments = max(muxed_segments, state.segments.muxed_segments)

    segments = %{
      video_segments: video_segments,
      audio_segments: audio_segments,
      muxed_segments: muxed_segments
    }

    %{state | segments: segments, stopped?: state.should_stop?.(segments)}
  end

  @impl true
  def remove(parent_id, resource_name, context, state) do
    {result, file_storage} =
      FileStorage.remove(parent_id, resource_name, context, state.file_storage)

    {result, %{state | file_storage: file_storage}}
  end
end
