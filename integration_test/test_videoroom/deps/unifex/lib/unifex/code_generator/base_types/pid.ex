defmodule Unifex.CodeGenerator.BaseTypes.Pid do
  @moduledoc """
  Module implementing `Unifex.CodeGenerator.BaseType` behaviour for Erlang PIDs.

  Implemented both for NIF and CNode as function parameter as well as return type.
  """
  use Unifex.CodeGenerator.BaseType

  @impl true
  def generate_native_type(_ctx) do
    ~g<UnifexPid>
  end

  defmodule NIF do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<enif_make_pid(env, &#{name})>
    end

    @impl true
    def generate_arg_parse(arg, var_name, _ctx) do
      ~g<enif_get_local_pid(env, #{arg}, &#{var_name})>
    end
  end

  defmodule CNode do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<ei_x_encode_pid(out_buff, &#{name});>
    end
  end
end
