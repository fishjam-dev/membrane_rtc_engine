defmodule Unifex.CodeGenerator.BaseTypes.String do
  @moduledoc """
  Module implementing `Unifex.CodeGenerator.BaseType` behaviour for strings.

  They are represented by NULL-terminated char arrays.

  Implemented both for NIF and CNode as function parameter as well as return type.
  """
  use Unifex.CodeGenerator.BaseType

  @impl true
  def ptr_level(_ctx), do: 1

  @impl true
  def generate_native_type(ctx) do
    optional_const = if ctx.mode == :const, do: "const ", else: ""
    ~g<char #{optional_const}*>
  end

  @impl true
  def generate_initialization(name, _ctx) do
    ~g<#{name} = NULL;>
  end

  @impl true
  def generate_destruction(name, _ctx) do
    ~g<unifex_free(#{name});>
  end

  defmodule NIF do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType
    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<unifex_string_to_term(env, #{name})>
    end

    @impl true
    def generate_arg_parse(arg, var_name, _ctx) do
      ~g<unifex_string_from_term(env, #{arg}, &#{var_name})>
    end
  end

  defmodule CNode do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<ei_x_encode_binary(out_buff, #{name}, strlen(#{name}));>
    end

    @impl true
    def generate_arg_parse(arg, var_name, _ctx) do
      ~g"""
      ({
        int type;
        int size;
        long len;
        ei_get_type(#{arg}->buff, #{arg}->index, &type, &size);
        size = size + 1; // for NULL byte
        #{var_name} = (char *)malloc(sizeof(char) * size);
        memset(#{var_name}, 0, size);
        ei_decode_binary(#{arg}->buff, #{arg}->index, #{var_name}, &len);
      })
      """
    end
  end
end
