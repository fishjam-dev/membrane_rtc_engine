defmodule Unifex.CodeGenerator.BaseTypes.Default do
  @moduledoc """
  Default `Unifex.CodeGenerator.BaseType` implementation for all types.

  If a callback is not implemented in a type-specific implementation,
  it defaults to this one.
  """
  use Unifex.CodeGenerator.BaseType

  @impl true
  def ptr_level(_ctx), do: 0

  @impl true
  def generate_native_type(ctx) do
    ~g<#{ctx.type}>
  end

  @impl true
  def generate_initialization(_name, _ctx) do
    ""
  end

  @impl true
  def generate_destruction(_name, _ctx) do
    ""
  end

  defmodule NIF do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, ctx) do
      ~g<enif_make_#{ctx.type}(env, #{name})>
    end

    @impl true
    def generate_arg_parse(argument, variable, ctx) do
      ~g<enif_get_#{ctx.type}(env, #{argument}, &#{variable})>
    end
  end

  defmodule CNode do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_parse(argument, name, ctx) do
      ~g<ei_decode_#{ctx.type}(#{argument}-\>buff, #{argument}-\>index, &#{name})>
    end

    @impl true
    def generate_arg_serialize(name, ctx) do
      ~g<ei_x_encode_#{ctx.type}(out_buff, #{name});>
    end
  end
end
