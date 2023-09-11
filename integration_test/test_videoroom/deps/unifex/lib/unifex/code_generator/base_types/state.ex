defmodule Unifex.CodeGenerator.BaseTypes.State do
  @moduledoc """
  Module implementing `Unifex.CodeGenerator.BaseType` behaviour for Unifex state.

  To use it, specify the state type with `Unifex.Specs.DSL.state_type/1`.
  """
  use Unifex.CodeGenerator.BaseType

  @impl true
  def ptr_level(_ctx), do: 1

  @impl true
  def generate_native_type(_ctx) do
    ~g<UnifexState*>
  end

  defmodule NIF do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<unifex_make_resource(env, #{name})>
    end

    @impl true
    def generate_arg_parse(arg, var_name, _ctx) do
      ~g<enif_get_resource(env, #{arg}, STATE_RESOURCE_TYPE, (void **)&#{var_name})>
    end
  end

  defmodule CNode do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_parse(_argument, variable, _ctx) do
      ~g"""
      ({
        #{variable} = (UnifexState*)env->state;
        0;
      })
      """
    end

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g"""
      {
        UnifexState* unifex_state = #{name};
        env->state = unifex_state;
      };
      """
    end
  end
end
