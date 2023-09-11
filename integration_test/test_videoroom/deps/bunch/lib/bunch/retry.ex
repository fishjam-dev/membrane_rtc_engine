defmodule Bunch.Retry do
  @moduledoc """
  A bunch of helpers for handling scenarios when some actions should be repeated
  until it succeeds.
  """

  @typedoc """
  Possible options for `retry/3`.
  """
  @type retry_option_t ::
          {:times, non_neg_integer()}
          | {:duration, milliseconds :: pos_integer}
          | {:delay, milliseconds :: pos_integer}

  @doc """
  Calls `fun` function until `arbiter` function decides to stop.

  Possible options are:
  - times - limits amount of retries (first evaluation is not considered a retry)
  - duration - limits total time of execution of this function, but breaks only
  before subsequent retry
  - delay - introduces delay (`:timer.sleep/1`) before each retry

  ## Examples

      iex> {:ok, pid} = Agent.start_link(fn -> 0 end)
      iex> #{inspect(__MODULE__)}.retry(fn -> Agent.get_and_update(pid, &{&1, &1+1}) end, & &1 > 3)
      4
      iex> {:ok, pid} = Agent.start_link(fn -> 0 end)
      iex> #{inspect(__MODULE__)}.retry(fn -> Agent.get_and_update(pid, &{&1, &1+1}) end, & &1 > 3, times: 10)
      4
      iex> {:ok, pid} = Agent.start_link(fn -> 0 end)
      iex> #{inspect(__MODULE__)}.retry(fn -> Agent.get_and_update(pid, &{&1, &1+1}) end, & &1 > 3, times: 2)
      2
      iex> {:ok, pid} = Agent.start_link(fn -> 0 end)
      iex> #{inspect(__MODULE__)}.retry(
      ...> fn -> :timer.sleep(100); Agent.get_and_update(pid, &{&1, &1+1}) end,
      ...> & &1 > 3,
      ...> duration: 150
      ...> )
      1
      iex> {:ok, pid} = Agent.start_link(fn -> 0 end)
      iex> #{inspect(__MODULE__)}.retry(
      ...> fn -> :timer.sleep(30); Agent.get_and_update(pid, &{&1, &1+1}) end,
      ...> & &1 > 3,
      ...> duration: 80, delay: 20
      ...> )
      1

  """
  @spec retry(
          fun :: (() -> res),
          arbiter :: (res -> stop? :: boolean),
          options :: [retry_option_t()]
        ) :: res
        when res: any()
  def retry(fun, arbiter, options \\ []) do
    times = options |> Keyword.get(:times, :infinity)
    duration = options |> Keyword.get(:duration, :infinity)
    delay = options |> Keyword.get(:delay, 0)
    fun |> do_retry(arbiter, times, duration, delay, 0, System.monotonic_time(:millisecond))
  end

  defp do_retry(fun, arbiter, times, duration, delay, retries, init_time) do
    ret = fun.()

    if not arbiter.(ret) and times > retries &&
         duration > System.monotonic_time(:millisecond) - init_time + delay do
      :timer.sleep(delay)
      fun |> do_retry(arbiter, times, duration, delay, retries + 1, init_time)
    else
      ret
    end
  end
end
