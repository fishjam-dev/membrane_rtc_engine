defmodule Unifex.CodeGenerators.NIF do
  @moduledoc """
  Generates NIF boilerplate based on `Unifex.Specs`.
  """

  @behaviour Unifex.CodeGenerator

  import Unifex.CodeGenerator.Utils, only: [sigil_g: 2]
  alias Unifex.{CodeGenerator, InterfaceIO, Specs}
  alias Unifex.CodeGenerator.{BaseType, Utils}
  alias Unifex.CodeGenerators.Common

  @impl CodeGenerator
  def identification_constant(), do: "BUNDLEX_NIF"

  @impl CodeGenerator
  def interface_io_name(), do: "nif"

  @impl CodeGenerator
  def generate_header(specs) do
    ctx = Common.create_ctx(specs)

    ~g"""
    #pragma once

    #include <stdio.h>
    #include <stdint.h>
    #include <erl_nif.h>
    #include <unifex/unifex.h>
    #include <unifex/payload.h>
    #include "#{InterfaceIO.user_header_path(specs.name)}"

    #ifdef __cplusplus
    extern "C" {
    #endif

    /*
     * Functions that manage lib and state lifecycle
     * Functions with 'unifex_' prefix are generated automatically,
     * the user have to implement rest of them.
     */

    #{generate_state_related_declarations(specs)}

    #{Utils.generate_enums_definitions(specs.enums,
    &Common.generate_enum_native_definition/2,
    ctx)}

    #{Utils.generate_structs_definitions(specs.structs,
    &generate_struct_native_definition/2,
    ctx)}

    /*
     * Declaration of native functions for module #{specs.module}.
     * The implementation have to be provided by the user.
     */

    #{Utils.generate_functions_declarations(specs.functions_args,
    &generate_implemented_function_declaration/2,
    ctx)}

    /*
     * Callbacks for nif lifecycle hooks.
     * Have to be implemented by user.
     */

    #{generate_nif_lifecycle_callbacks_declarations(specs.callbacks)}

    /*
     * Functions that create the defined output from Nif.
     * They are automatically generated and don't need to be implemented.
     */

    #{Utils.generate_functions_declarations(specs.functions_results,
    &generate_result_function_declaration/2,
    ctx)}

    /*
     * Functions that send the defined messages from Nif.
     * They are automatically generated and don't need to be implemented.
     */

    #{Utils.generate_functions_declarations(specs.sends,
    &generate_send_function_declaration/2,
    ctx)}

    #ifdef __cplusplus
    }
    #endif
    """
  end

  @impl CodeGenerator
  def generate_source(specs) do
    ctx = Common.create_ctx(specs)

    ~g"""
    #include "#{specs.name}.h"

    #{Utils.generate_functions(specs.functions_results, &generate_result_function/2, ctx)}
    #{Utils.generate_functions(specs.sends, &generate_send_function/2, ctx)}
    #{generate_state_related_functions(specs)}
    #{generate_nif_lifecycle_callbacks(specs)}
    #{Utils.generate_functions(specs.functions_args, &generate_export_function/2, ctx)}
    #{generate_erlang_boilerplate(specs)}
    """
  end

  defp generate_implemented_function_declaration({name, args}, ctx) do
    args_declarations =
      [
        ~g<UnifexEnv* env>
        | Enum.flat_map(args, fn {name, type} ->
            BaseType.generate_declaration(type, name, NIF, ctx)
          end)
      ]
      |> Enum.join(", ")

    ~g<UNIFEX_TERM #{name}(#{args_declarations})>
  end

  defp generate_args_declarations(args, mode, ctx) do
    Enum.flat_map(args, fn {name, type} ->
      BaseType.generate_declaration(type, name, mode, NIF, ctx)
    end)
  end

  defp generate_result_function({name, result}, ctx) do
    declaration = generate_result_function_declaration({name, result}, ctx)
    {result, _meta} = generate_serialization(result, ctx)

    ~g"""
    #{declaration} {
      return #{result};
    }
    """
  end

  defp generate_result_function_declaration({name, result}, ctx) do
    {_result, meta} = generate_serialization(result, ctx)
    args = meta |> Keyword.get_values(:arg)
    labels = meta |> Keyword.get_values(:label)

    args_declarations =
      [~g<UnifexEnv* env> | generate_args_declarations(args, :const_unless_ptr_on_ptr, ctx)]
      |> Enum.join(", ")

    ~g<UNIFEX_TERM #{[name, :result | labels] |> Enum.join("_")}(#{args_declarations})>
  end

  defp generate_send_function(sends, ctx) do
    declaration = generate_send_function_declaration(sends, ctx)
    {result, _meta} = generate_serialization(sends, ctx)

    ~g"""
    #{declaration} {
      ERL_NIF_TERM term = #{result};
      return unifex_send(env, &pid, term, flags);
    }
    """
  end

  defp generate_send_function_declaration(sends, ctx) do
    {_result, meta} = generate_serialization(sends, ctx)
    args = meta |> Keyword.get_values(:arg)
    labels = meta |> Keyword.get_values(:label)

    args_declarations =
      [
        ~g<UnifexEnv* env>,
        ~g<UnifexPid pid>,
        ~g<int flags>
        | generate_args_declarations(args, :const_unless_ptr_on_ptr, ctx)
      ]
      |> Enum.join(", ")

    ~g<int #{[:send | labels] |> Enum.join("_")}(#{args_declarations})>
  end

  defp generate_export_function({name, args}, ctx) do
    result_var = "result"
    exit_label = "exit_export_#{name}"

    maybe_unused_args =
      Utils.generate_maybe_unused_args_statements(["argc", "argv"]) |> Enum.join("\n")

    args_declaration =
      args
      |> Enum.flat_map(fn {name, type} -> BaseType.generate_declaration(type, name, NIF, ctx) end)
      |> Enum.map_join("\n", &~g<#{&1};>)

    args_initialization =
      Enum.map_join(args, "\n", fn {name, type} ->
        BaseType.generate_initialization(type, name, NIF, ctx)
      end)

    args_parsing =
      args
      |> Enum.with_index()
      |> Enum.map_join("\n", fn {{name, type}, i} ->
        postproc_fun = fn arg_getter ->
          ~g"""
          if(!#{arg_getter}) {
            #{result_var} = unifex_raise_args_error(env, "#{name}", "#{inspect(type)}");
            goto #{exit_label};
          }
          """
        end

        BaseType.generate_arg_parse(type, name, "argv[#{i}]", postproc_fun, NIF, ctx)
      end)

    args_destruction =
      args
      |> Enum.map(fn {name, type} -> BaseType.generate_destruction(type, name, NIF, ctx) end)
      |> Enum.reject(&("" == &1))
      |> Enum.join("\n")

    args_names =
      Enum.flat_map(args, fn {name, type} -> BaseType.generate_arg_name(type, name, NIF, ctx) end)

    ~g"""
    static ERL_NIF_TERM export_#{name}(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
      #{maybe_unused_args}
      ERL_NIF_TERM #{result_var};
      #{generate_unifex_env()}
      #{args_declaration}

      #{args_initialization}

      #{args_parsing}

      #{result_var} = #{name}(#{[:unifex_env | args_names] |> Enum.join(", ")});
      goto #{exit_label};
    #{exit_label}:
      #{args_destruction}
      return result;
    }
    """
  end

  defp generate_state_related_declarations(%Specs{state_type: nil}) do
    ~g<>
  end

  defp generate_state_related_declarations(%Specs{state_type: state_type}) do
    ~g"""
    typedef #{state_type} UnifexState;

    /**
     * Allocates the state struct. Have to be paired with 'unifex_release_state' call
     */
    UnifexState* unifex_alloc_state(UnifexEnv* env);

    /**
     * Removes a reference to the state object.
     * The state is destructed when the last reference is removed.
     * Each call to 'unifex_release_state' must correspond to a previous
     * call to 'unifex_alloc_state' or 'unifex_keep_state'.
     */
    void unifex_release_state(UnifexEnv* env, UnifexState* state);

    /**
     * Increases reference count of state object.
     * Each call has to be balanced by 'unifex_release_state' call
     */
    void unifex_keep_state(UnifexEnv* env, UnifexState* state);

    /**
     * Callback called when the state struct is destroyed. It should
     * be responsible for releasing any resources kept inside state.
     */
    void handle_destroy_state(UnifexEnv* env, UnifexState* state);
    """
  end

  defp generate_state_related_functions(%Specs{state_type: nil}) do
    ~g<>
  end

  defp generate_state_related_functions(%Specs{}) do
    ~g"""
    ErlNifResourceType *STATE_RESOURCE_TYPE;

    UnifexState* unifex_alloc_state(UnifexEnv* env) {
      UNIFEX_UNUSED(env);
      return (UnifexState*) enif_alloc_resource(STATE_RESOURCE_TYPE, sizeof(UnifexState));
    }

    void unifex_release_state(UnifexEnv * env, UnifexState* state) {
      UNIFEX_UNUSED(env);
      enif_release_resource(state);
    }

    void unifex_keep_state(UnifexEnv * env, UnifexState* state) {
      UNIFEX_UNUSED(env);
      enif_keep_resource(state);
    }

    static void destroy_state(ErlNifEnv* env, void* value) {
      UnifexState* state = (UnifexState*) value;
      #{generate_unifex_env()}
      handle_destroy_state(unifex_env, state);
    }
    """
  end

  defp generate_nif_lifecycle_callbacks_declarations(callbacks) do
    callbacks
    |> Enum.map_join("\n", fn
      {:load, fun_name} ->
        ~g"int #{fun_name}(UnifexEnv * env, void ** priv_data);"

      {:upgrade, fun_name} ->
        ~g"int #{fun_name}(UnifexEnv * env, void ** priv_data, void **old_priv_data);"

      {:unload, fun_name} ->
        ~g"void #{fun_name}(UnifexEnv * env, void * priv_data);"
    end)
  end

  defp state_resource_type_initialization(%Specs{state_type: nil}) do
    ~g<>
  end

  defp state_resource_type_initialization(%Specs{}) do
    ~g"""
      STATE_RESOURCE_TYPE =
        enif_open_resource_type(env, NULL, "UnifexState", (ErlNifResourceDtor*) destroy_state, flags, NULL);
    """
  end

  defp generate_nif_lifecycle_callbacks(%Specs{module: nil}) do
    ~g<>
  end

  defp generate_nif_lifecycle_callbacks(%Specs{callbacks: callbacks} = specs) do
    load_result =
      case callbacks[:load] do
        nil -> "0"
        name -> ~g"#{name}(env, priv_data)"
      end

    load = ~g"""
    static int unifex_load_nif(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
      UNIFEX_UNUSED(load_info);
      UNIFEX_UNUSED(priv_data);

      ErlNifResourceFlags flags = (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);

      #{state_resource_type_initialization(specs)}

      UNIFEX_PAYLOAD_GUARD_RESOURCE_TYPE =
        enif_open_resource_type(env, NULL, "UnifexPayloadGuard", (ErlNifResourceDtor*) unifex_payload_guard_destructor, flags, NULL);

      return #{load_result};
    }
    """

    upgrade =
      case callbacks[:upgrade] do
        nil ->
          ~g""

        name ->
          ~g"""
          static int unifex_upgrade_nif(ErlNifEnv * env, void ** priv_data, void **old_priv_data, ERL_NIF_TERM load_info) {
            UNIFEX_UNUSED(load_info);
            return #{name}(env, priv_data, old_priv_data);
          }
          """
      end

    unload =
      case callbacks[:unload] do
        nil ->
          ~g""

        name ->
          ~g"""
          static void unifex_unload_nif(ErlNifEnv* env, void* priv_data) {
            #{name}(env, priv_data);
          }
          """
      end

    [load, upgrade, unload]
    |> Enum.join("\n")
  end

  defp generate_erlang_boilerplate(%Specs{module: nil}) do
    ~g<>
  end

  defp generate_erlang_boilerplate(specs) do
    printed_funcs =
      specs.functions_args
      |> Enum.map_join(",\n", fn {name, args} ->
        arity = length(args)

        flags =
          case specs.dirty_functions[{name, arity}] do
            :cpu -> ~g<ERL_NIF_DIRTY_JOB_CPU_BOUND>
            :io -> ~g<ERL_NIF_DIRTY_JOB_IO_BOUND>
            nil -> ~g<0>
          end

        ~g<{"unifex_#{name}", #{arity}, export_#{name}, #{flags}}>
      end)

    # Erlang used to have reload callback. It is unsupported from OTP 20
    # Its entry in ERL_NIF_INIT parameters is always NULL
    callback_pointers =
      [:deprecated_reload, :upgrade, :unload]
      |> Enum.map_join(", ", fn hook ->
        case specs.callbacks[hook] do
          nil -> "NULL"
          _callback -> "unifex_#{hook}_nif"
        end
      end)

    ~g"""
    static ErlNifFunc nif_funcs[] =
    {
      #{printed_funcs}
    };

    ERL_NIF_INIT(#{specs.module}.Nif, nif_funcs, unifex_load_nif, #{callback_pointers})
    """
  end

  defp generate_tuple_maker(content) do
    ~g<({
      const ERL_NIF_TERM terms[] = {
        #{content |> Enum.join(",\n")}
      };
      enif_make_tuple_from_array(env, terms, #{length(content)});
    })>
  end

  defp generate_unifex_env() do
    ~g<UnifexEnv *unifex_env = env;>
  end

  defp generate_serialization(specs, ctx) do
    Utils.generate_serialization(specs, %{
      arg_serializer: fn type, name -> BaseType.generate_arg_serialize(type, name, NIF, ctx) end,
      tuple_serializer: &generate_tuple_maker/1
    })
  end

  defp generate_struct_native_definition(struct_data, ctx) do
    Unifex.CodeGenerators.Common.generate_struct_native_definition(struct_data, NIF, ctx)
  end
end
