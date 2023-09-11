defmodule Unifex.CodeGenerator.BaseTypes.Bool do
  @moduledoc """
  Module implementing `Unifex.CodeGenerator.BaseType` behaviour for boolean atoms.

  Booleans in native code are converted to int with value either 0 or 1.

  Implemented both for NIF and CNode as function parameter as well as return type.
  """
  use Unifex.CodeGenerator.BaseType

  @impl true
  def generate_native_type(_ctx) do
    ~g<int>
  end

  defmodule NIF do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<enif_make_atom(env, #{name} ? "true" : "false")>
    end

    @impl true
    def generate_arg_parse(arg_term, var_name, _ctx) do
      ~g<unifex_parse_bool(env, #{arg_term}, &#{var_name})>
    end
  end

  defmodule CNode do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<ei_x_encode_atom(out_buff, #{name} ? "true" : "false");>
    end

    @impl true
    def generate_arg_parse(arg, var_name, _ctx) do
      ~g"""
      ({
        int res = -1;
        char boolean_str[6];
        ei_decode_atom(#{arg}->buff, #{arg}->index, boolean_str);

        if (strcmp(boolean_str, "true") == 0) {
          #{var_name} = 1;
          res = 0;
        }
        else if (strcmp(boolean_str, "false") == 0) {
          #{var_name} = 0;
          res = 0;
        }
        res;
      })
      """
    end
  end
end
