defmodule Bunch.Type do
  @moduledoc """
  A bunch of commonly used types.
  """

  @typedoc """
  Represents result of an operation that may succeed or fail.
  """
  @type try_t :: :ok | {:error, reason :: any}

  @typedoc """
  Represents result of an operation that may return something or fail.
  """
  @type try_t(value) :: {:ok, value} | {:error, reason :: any}

  @typedoc """
  Represents a value along with state.
  """
  @type stateful_t(value, state) :: {value, state}

  @typedoc """
  Represents a `t:try_t/0` value along with state.
  """
  @type stateful_try_t(state) :: stateful_t(try_t, state)

  @typedoc """
  Represents a `t:try_t/1` value along with state.
  """
  @type stateful_try_t(value, state) :: stateful_t(try_t(value), state)
end
