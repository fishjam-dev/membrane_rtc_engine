defmodule Unifex.CodeGenerator.BaseTypes.Int do
  @moduledoc """
  Module implementing `Unifex.CodeGenerator.BaseType` behaviour for integers.

  Implemented both for NIF and CNode as function parameter as well as return type.
  """

  defmodule CNode do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_parse(argument, name, _ctx) do
      ~g"""
      ({
        long long tmp_longlong;
        int result = ei_decode_longlong(#{argument}->buff, #{argument}->index, &tmp_longlong);
        #{name} = (int)tmp_longlong;
        result;
      })
      """
    end

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g"""
      ({
      int tmp_int = #{name};
      ei_x_encode_longlong(out_buff, (long long)tmp_int);
      });
      """
    end
  end
end
