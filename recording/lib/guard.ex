defmodule Membrane.RTC.Engine.Endpoint.Recording.Guard do
  @moduledoc false

  alias Membrane.RTC.Engine.Endpoint.Recording.Storage

  @type storage :: {module(), opts :: any()}

  @spec close_recording([storage()], String.t()) :: [storage()]
  def close_recording(stores, recording_id) do
    case List.keytake(stores, Storage.File, 0) do
      nil ->
        stores

      {file_store, other_stores} ->
        valid_stores = close_stores(file_store, other_stores, recording_id)
        [file_store | valid_stores]
    end
  end

  defp close_stores({file_storage, file_opts}, stores, recording_id) do
    files = file_storage.list_files(file_opts)

    if map_size(files) > 0 do
      Enum.filter(stores, fn {module, opts} ->
        module.on_close(files, recording_id, opts) == :ok
      end)
    else
      stores
    end
  end
end
