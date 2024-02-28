defmodule Membrane.RTC.Engine.Support.CrashingRecordingStorage do
  @moduledoc false

  @behaviour Membrane.RTC.Engine.Endpoint.Recording.Storage

  @impl true
  def get_sink(_config) do
    %Membrane.Debug.Sink{
      handle_buffer: fn _buf -> raise "CRASHING SINK CRASHES!!!!!!!!!! :O" end
    }
  end

  @impl true
  def save_object(_config) do
    :ok
  end
end
