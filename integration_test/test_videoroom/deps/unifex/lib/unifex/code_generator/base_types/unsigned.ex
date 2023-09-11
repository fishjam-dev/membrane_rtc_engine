defmodule Unifex.CodeGenerator.BaseTypes.Unsigned do
  @moduledoc """
  Module implementing `Unifex.CodeGenerator.BaseType` behaviour for unsigned int.

  Implemented both for NIF and CNode as function parameter as well as return type.
  """
  use Unifex.CodeGenerator.BaseType

  @impl true
  def generate_native_type(_ctx) do
    ~g<unsigned int>
  end

  defmodule NIF do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<enif_make_uint(env, #{name})>
    end

    @impl true
    def generate_arg_parse(arg_term, var_name, _ctx) do
      ~g<enif_get_uint(env, #{arg_term}, &#{var_name})>
    end
  end

  defmodule CNode do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g"""
      ({
        unsigned int tmp_uint = #{name};
        ei_x_encode_ulonglong(out_buff, (unsigned long long)tmp_uint);
      });
      """
    end

    @impl true
    def generate_arg_parse(argument, name, _ctx) do
      ~g"""
      ({
        unsigned long long tmp_ulonglong;
        int result = ei_decode_ulonglong(#{argument}->buff, #{argument}->index, &tmp_ulonglong);
        #{name} = (unsigned int)tmp_ulonglong;
        result;
      })
      """
    end
  end
end
