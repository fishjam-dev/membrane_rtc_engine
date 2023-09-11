defmodule Qex do
  @moduledoc ~S"""

  A `:queue` wrapper with improvements in API and addition of Protocol implementations

  ## Protocols

  `Inspect`, `Collectable` and `Enumerable` are implemented

      iex> inspect Qex.new
      "#Qex<[]>"

      iex> Enum.count Qex.new(1..5)
      5

      iex> Enum.empty? Qex.new
      true

      iex> Enum.map Qex.new([1, 2, 3]), &(&1 + 1)
      [2, 3, 4]

      iex> inspect Enum.into(1..5, %Qex{})
      "#Qex<[1, 2, 3, 4, 5]>"
  """

  @opaque t(type) :: %__MODULE__{:data => :queue.queue(type)}
  @opaque t() :: %__MODULE__{:data => :queue.queue()}

  defstruct data: :queue.new

  @doc """
  Create a new queue from a range

      iex> inspect Qex.new(1..3)
      "#Qex<[1, 2, 3]>"

  Create a new queue from a list

      iex> inspect Qex.new([1, 2, 3])
      "#Qex<[1, 2, 3]>"
  """
  @spec new([term] | Range.t) :: t
  def new(init_data \\ [])

  def new(x..y) do
    %__MODULE__{data: :queue.from_list(Enum.to_list(x..y))}
  end

  def new(list) do
    %__MODULE__{data: :queue.from_list(list)}
  end

  @doc """
  Add an element to the back of the queue

      iex> q = Qex.new([:mid])
      iex> Enum.to_list Qex.push(q, :back)
      [:mid, :back]
  """
  @spec push(t, term) :: t
  def push(%__MODULE__{data: q}, item) do
    %__MODULE__{data: :queue.in(item, q)}
  end

  @doc """
  Add an element to the front of the queue

      iex> q = Qex.new([:mid])
      iex> Enum.to_list Qex.push_front(q, :front)
      [:front, :mid]
  """
  @spec push_front(t, term) :: t
  def push_front(%__MODULE__{data: q}, item) do
    %__MODULE__{data: :queue.in_r(item, q)}
  end

  @doc """
  Get and remove an element from the front of the queue

      iex> q = Qex.new([:front, :mid])
      iex> {{:value, item}, _q} = Qex.pop(q)
      iex> item
      :front

      iex> q = Qex.new
      iex> {empty, _q} = Qex.pop(q)
      iex> empty
      :empty
  """
  @spec pop(t) :: {{:value, term}, t} | {:empty, t}
  def pop(%__MODULE__{data: q}) do
    case :queue.out(q) do
      {{:value, v}, q} -> {{:value, v}, %__MODULE__{data: q}}
      {:empty, q} -> {:empty, %__MODULE__{data: q}}
    end
  end

  @spec pop!(t) :: {term, t} | no_return
  def pop!(%__MODULE__{data: q}) do
    case :queue.out(q) do
      {{:value, v}, q} -> {v, %__MODULE__{data: q}}
      {:empty, _q} -> raise "Queue is empty"
    end
  end

  @doc """
  Get and remove an element from the back of the queue

      iex> q = Qex.new([:mid, :back])
      iex> {{:value, item}, _q} = Qex.pop_back(q)
      iex> item
      :back

      iex> q = Qex.new
      iex> {empty, _q} = Qex.pop_back(q)
      iex> empty
      :empty
  """
  @spec pop_back(t) :: {{:value, term}, t} | {:empty, t}
  def pop_back(%__MODULE__{data: q}) do
    case :queue.out_r(q) do
      {{:value, v}, q} -> {{:value, v}, %__MODULE__{data: q}}
      {:empty, q} -> {:empty, %__MODULE__{data: q}}
    end
  end

  @spec pop_back!(t) :: {term, t} | no_return
  def pop_back!(%__MODULE__{data: q}) do
    case :queue.out_r(q) do
      {{:value, v}, q} -> {v, %__MODULE__{data: q}}
      {:empty, _q} -> raise "Queue is empty"
    end
  end

  @doc """
  Reverse a queue

      iex> q = Qex.new(1..3)
      iex> Enum.to_list q
      [1, 2, 3]
      iex> Enum.to_list Qex.reverse(q)
      [3, 2, 1]
  """
  @spec reverse(t) :: t
  def reverse(%__MODULE__{data: q}) do
    %__MODULE__{data: :queue.reverse(q)}
  end

  @doc """
  Split a queue into two, the front n items are put in the first queue

      iex> q = Qex.new 1..5
      iex> {q1, q2} = Qex.split(q, 3)
      iex> Enum.to_list q1
      [1, 2, 3]
      iex> Enum.to_list q2
      [4, 5]
  """
  @spec split(t, pos_integer) :: {t, t}
  def split(%__MODULE__{data: q}, n) do
    with {q1, q2} <- :queue.split(n, q) do
      {%__MODULE__{data: q1}, %__MODULE__{data: q2}}
    end
  end

  @doc """
  Join two queues together

      iex> q1 = Qex.new 1..3
      iex> q2 = Qex.new 4..5
      iex> Enum.to_list Qex.join(q1, q2)
      [1, 2, 3, 4, 5]
  """
  @spec join(t, t) :: t
  def join(%__MODULE__{data: q1}, %__MODULE__{data: q2}) do
    %__MODULE__{data: :queue.join(q1, q2)}
  end
  
  @doc """
  Return the first item in the queue in {:value, term} tuple,
  return :empty if the queue is empty
  
      iex> q1 = Qex.new 1..3
      iex> Qex.first(q1)
      {:value, 1}
      iex> q2 = Qex.new []
      iex> Qex.first(q2)
      :empty
  """
  @spec first(t) :: {:value, term} | :empty
  def first(%__MODULE__{data: q}) do
    :queue.peek(q)
  end

  @doc """
  Retun the first item in the queue, raise if it's empty
  
      iex> q1 = Qex.new 1..3
      iex> Qex.first!(q1)
      1
  """
  @spec first!(t) :: term | no_return
  def first!(%__MODULE__{data: q}) do
    case :queue.peek(q) do
      {:value, v} -> v
      :empty -> raise "Queue is empty"
    end
  end
  
  @doc """
  Return the last item in the queue in {:value, term} tuple,
  return :empty if the queue is empty
  
      iex> q1 = Qex.new 1..3
      iex> Qex.last(q1)
      {:value, 3}
      iex> q2 = Qex.new []
      iex> Qex.last(q2)
      :empty
  """
  @spec last(t) :: {:value, term} | :empty
  def last(%__MODULE__{data: q}) do
    :queue.peek_r(q)
  end

  @doc """
  Retun the last item in the queue, raise if it's empty
  
      iex> q1 = Qex.new 1..3
      iex> Qex.last!(q1)
      3
  """
  @spec last!(t) :: term | no_return
  def last!(%__MODULE__{data: q}) do
    case :queue.peek_r(q) do
      {:value, v} -> v
      :empty -> raise "Queue is empty"
    end
  end
end
