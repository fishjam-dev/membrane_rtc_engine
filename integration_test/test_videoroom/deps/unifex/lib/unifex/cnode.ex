defmodule Unifex.CNode do
  @moduledoc """
  Wraps Bundlex.CNode functionalities to support Unifex-specific CNode behaviours
  """

  require Bundlex.CNode

  @enforce_keys [:server, :node, :bundlex_cnode]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          server: pid,
          node: node,
          bundlex_cnode: Bundlex.CNode.t()
        }

  @type on_start_t :: {:ok, t} | {:error, :spawn_cnode | :connect_to_cnode}

  @doc """
  Spawns and connects to CNode `cnode_name`.

  For details, see `Bundlex.CNode.start_link/2`.
  """
  defmacro start_link(native_name) do
    quote do
      require Bundlex.CNode

      unquote(native_name)
      |> Bundlex.CNode.start_link()
      |> unquote(__MODULE__).wrap_start_result()
    end
  end

  @doc """
  Works the same way as `start_link/1`, but does not link to CNode's associated
  server.
  """
  defmacro start(native_name) do
    quote do
      require Bundlex.CNode

      unquote(native_name)
      |> Bundlex.CNode.start()
      |> unquote(__MODULE__).wrap_start_result()
    end
  end

  @doc """
  Spawns and connects to CNode `cnode_name` from application `app`.

  For details, see `Bundlex.CNode.start_link/2`.
  """
  @spec start_link(app :: Application.app(), Unifex.Specs.native_name_t()) :: on_start_t
  def start_link(app, native_name) do
    Bundlex.CNode.start_link(app, native_name) |> wrap_start_result()
  end

  @doc """
  Works the same way as `start_link/2`, but does not link to CNode's associated
  server.
  """
  @spec start(app :: Application.app(), Unifex.Specs.native_name_t()) :: on_start_t
  def start(app, native_name) do
    Bundlex.CNode.start(app, native_name) |> wrap_start_result()
  end

  @doc """
  Disconnects from CNode.
  """
  @spec stop(t) :: :ok | {:error, :disconnect_cnode}
  def stop(%__MODULE__{bundlex_cnode: cnode}) do
    Bundlex.CNode.stop(cnode)
  end

  @doc """
  Starts monitoring CNode from the calling process.
  """
  @spec monitor(t) :: reference
  def monitor(%__MODULE__{bundlex_cnode: cnode}) do
    Bundlex.CNode.monitor(cnode)
  end

  @doc """
  Makes a synchronous call to CNode and waits for its reply.

  If the response doesn't come in within `timeout`, error is raised.
  Messages are exchanged directly (without interacting with CNode's associated
  server).
  """
  @spec call(t, fun_name :: atom, args :: list, timeout :: non_neg_integer | :infinity) ::
          response :: term
  def call(%__MODULE__{bundlex_cnode: cnode}, fun_name, args \\ [], timeout \\ 5000) do
    msg = [fun_name | args] |> List.to_tuple()

    cnode
    |> Bundlex.CNode.call(msg, timeout)
    |> handle_response()
  end

  @doc false
  @spec wrap_start_result(Bundlex.CNode.on_start_t()) :: on_start_t
  def wrap_start_result({:ok, %Bundlex.CNode{} = bundlex_cnode}) do
    {:ok,
     %__MODULE__{
       server: bundlex_cnode.server,
       node: bundlex_cnode.node,
       bundlex_cnode: bundlex_cnode
     }}
  end

  def wrap_start_result(start_result) do
    start_result
  end

  defp handle_response({:result, result}), do: result

  defp handle_response({:error, {:undefined_function, fun_name}}) do
    raise "Undefined Unifex CNode function: #{fun_name}"
  end

  defp handle_response({:raise, message}) do
    raise message
  end
end
