defmodule Membrane.RTC.Engine.TrackManager do
  @moduledoc """
  TBD
  """
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [])
  end

  @impl true
  def init(_) do
    {:ok, %{}}
  end

  def handle_info({:vad_notification, audio_track_id, val}) do
    {:no_reply, state}
  end
end
