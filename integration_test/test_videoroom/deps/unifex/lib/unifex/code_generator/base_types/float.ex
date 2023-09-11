defmodule Unifex.CodeGenerator.BaseTypes.Float do
  @moduledoc """
  Module implementing `Unifex.CodeGenerator.BaseType` behaviour for floats.

  Implemented both for NIF and CNode as function parameter as well as return type.
  """

  use Unifex.CodeGenerator.BaseType

  @impl true
  def generate_native_type(_ctx) do
    ~g<double>
  end

  defmodule NIF do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<enif_make_double(env, #{name})>
    end

    @impl true
    def generate_arg_parse(argument, variable, _ctx) do
      ~g<enif_get_double(env, #{argument}, &#{variable})>
    end
  end

  defmodule CNode do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_parse(argument, name, _ctx) do
      ~g<ei_decode_double(#{argument}-\>buff, #{argument}-\>index, &#{name})>
    end

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<ei_x_encode_double(out_buff, #{name});>
    end
  end
end
