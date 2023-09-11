defmodule Bunch.Struct do
  @moduledoc """
  A bunch of functions for easier manipulation on structs.
  """

  use Bunch

  import Kernel, except: [get_in: 2, put_in: 2, update_in: 3, get_and_update_in: 3, pop_in: 2]

  @compile {:inline, map_keys: 1}

  @gen_common_docs fn fun_name ->
    """
    Wraps `Bunch.Access.#{fun_name}` to make it work with structs that do not
    implement `Access` behaviour.
    """
  end

  @doc """
  #{@gen_common_docs.("get_in/2")}
  """
  @spec get_in(struct, Access.key() | [Access.key()]) :: Access.value()
  def get_in(struct, keys), do: struct |> Bunch.Access.get_in(keys |> map_keys())

  @doc """
  #{@gen_common_docs.("put_in/3")}
  """
  @spec put_in(struct, Access.key() | [Access.key()], Access.value()) :: Access.value()
  def put_in(struct, keys, v), do: struct |> Bunch.Access.put_in(keys |> map_keys(), v)

  @doc """
  #{@gen_common_docs.("update_in/3")}
  """
  @spec update_in(struct, Access.key() | [Access.key()], (Access.value() -> Access.value())) ::
          struct
  def update_in(struct, keys, f), do: struct |> Bunch.Access.update_in(keys |> map_keys(), f)

  @doc """
  #{@gen_common_docs.("get_and_update_in/3")}
  """
  @spec get_and_update_in(struct, Access.key() | [Access.key()], (a -> {b, a})) :: {b, struct}
        when a: Access.value(), b: any
  def get_and_update_in(struct, keys, f),
    do: struct |> Bunch.Access.get_and_update_in(keys |> map_keys(), f)

  @doc """
  #{@gen_common_docs.("pop_in/2")}
  """
  @spec pop_in(struct, Access.key() | [Access.key()]) :: {Access.value(), struct}
  def pop_in(struct, keys), do: struct |> Bunch.Access.pop_in(keys |> map_keys())

  @doc """
  #{@gen_common_docs.("delete_in/2")}
  """
  @spec delete_in(struct, Access.key() | [Access.key()]) :: struct
  def delete_in(struct, keys), do: struct |> Bunch.Access.delete_in(keys |> map_keys())

  @spec map_keys(Access.key() | [Access.key()]) :: [Access.access_fun(struct | map, term)]
  defp map_keys(keys), do: keys |> Bunch.listify() |> Enum.map(&Access.key(&1, nil))
end
