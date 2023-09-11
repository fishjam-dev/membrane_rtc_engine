defmodule Unifex.CodeGenerator.BaseTypes.Payload do
  @moduledoc """
  Module implementing `Unifex.CodeGenerator.BaseType` behaviour for payloads.

  Implemented both for NIF and CNode as function parameter as well as return type.
  """
  use Unifex.CodeGenerator.BaseType

  @impl true
  def ptr_level(_ctx), do: 1

  @impl true
  def generate_native_type(_ctx) do
    ~g<UnifexPayload *>
  end

  defmodule NIF do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_initialization(name, _ctx) do
      ~g<#{name} = (UnifexPayload *) unifex_alloc(sizeof (UnifexPayload));>
    end

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<unifex_payload_to_term(env, #{name})>
    end

    @impl true
    def generate_arg_parse(arg, var_name, _ctx) do
      ~g<unifex_payload_from_term(env, #{arg}, #{var_name})>
    end

    @impl true
    def generate_destruction(name, _ctx) do
      ~g"""
      unifex_payload_release(#{name});
      unifex_free(#{name});
      """
    end
  end

  defmodule CNode do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType

    @impl true
    def generate_initialization(name, _ctx) do
      ~g<#{name} = NULL;>
    end

    @impl true
    def generate_arg_serialize(name, _ctx) do
      ~g<unifex_payload_encode(env, out_buff, #{name});>
    end

    @impl true
    def generate_arg_parse(arg, var_name, _ctx) do
      ~g<unifex_payload_decode(env, #{arg}, &#{var_name})>
    end

    @impl true
    def generate_destruction(name, _ctx) do
      ~g"""
      if(#{name} && !#{name}->owned) {
        unifex_payload_release(#{name});
      }
      unifex_free(#{name});
      """
    end
  end
end
