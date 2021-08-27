defmodule Membrane.RTC.Test.EngineHelpers do
  @moduledoc false

  import ExUnit.Assertions

  @spec refute_media_event_broadcast(binary()) :: :ok
  def refute_media_event_broadcast(type) do
    receive do
      {_from, {:sfu_media_event, :broadcast, media_event}} ->
        case Jason.decode!(media_event) do
          %{"type" => ^type} ->
            flunk("""
            Expected not to receive a media event with type #{type}.
            Received:
              #{media_event}
            """)

          _ ->
            refute_media_event_broadcast(type)
        end
    after
      150 -> :ok
    end
  end

  @spec wait_for_media_event(binary(), binary()) :: map()
  def wait_for_media_event(type, peer_id) do
    assert_receive {_from, {:sfu_media_event, ^peer_id, media_event}}, 500

    case Jason.decode!(media_event) do
      %{"type" => ^type} = decoded_event -> decoded_event
      _ -> wait_for_media_event(type, peer_id)
    end
  end

  @spec wait_for_media_event_broadcast(binary()) :: map()
  def wait_for_media_event_broadcast(type) do
    assert_receive {_from, {:sfu_media_event, :broadcast, media_event}}, 500

    case Jason.decode!(media_event) do
      %{"type" => ^type} = decoded_event -> decoded_event
      _ -> wait_for_media_event_broadcast(type)
    end
  end

  @doc """
  Registers `self()` for message dispatches that are generated from unit tests
  of Engine functions called in the context of the test process.
  """
  @spec register_test_pid() :: {:ok, pid()}
  def register_test_pid,
    do: Registry.register(Membrane.RTC.Engine.get_registry_name(), self(), self())
end
