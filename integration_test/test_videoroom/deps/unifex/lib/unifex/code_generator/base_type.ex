defmodule Unifex.CodeGenerator.BaseType do
  @moduledoc """
  Behaviour and abstraction over type-specific code generation.

  When invoking callbacks for type `:type` and interface `Interface` it searches for
  a module that implements given callback in the following order:
  - `Unifex.CodeGenerator.BaseTypes.Type.Interface`
  - `Unifex.CodeGenerator.BaseTypes.Type`
  - `Unifex.CodeGenerator.BaseTypes.Default.Interface`
  """
  import Unifex.CodeGenerator.Utils, only: [sigil_g: 2]
  alias Unifex.CodeGenerator
  alias Unifex.CodeGenerator.BaseTypes

  @type t :: atom | {:list, atom}
  @type arg_parse_ctx_t :: %{
          result_var: CodeGenerator.code_t(),
          exit_label: CodeGenerator.code_t()
        }

  @doc """
  Provides a way to convert native variable `name` into `UNIFEX_TERM`
  """
  @callback generate_arg_serialize(name :: atom, ctx :: map) :: CodeGenerator.code_t()

  @doc """
  Generates an initialization of variable content. Should be paired with `c:generate_destruction/2`
  """
  @callback generate_initialization(name :: atom, ctx :: map) :: CodeGenerator.code_t()

  @doc """
  Generates a destruction of variable content. Should be paired with `c:generate_initialization/2`
  """
  @callback generate_destruction(name :: atom, ctx :: map) :: CodeGenerator.code_t()

  @doc """
  Generates a native counterpart for the type.
  """
  @callback generate_native_type(ctx :: map) :: CodeGenerator.code_t() | [CodeGenerator.code_t()]

  @doc """
  Generates function call parsing UNIFEX_TERM `argument` into the native variable with name `variable`. Function should
  return boolean value.
  """
  @callback generate_arg_parse(
              argument :: CodeGenerator.code_t(),
              variable :: CodeGenerator.code_t(),
              ctx :: map
            ) ::
              CodeGenerator.code_t()

  @doc """
  Returns level of pointer nesting of native type.
  """
  @callback ptr_level(ctx :: map) :: integer()

  @optional_callbacks generate_arg_serialize: 2,
                      generate_initialization: 2,
                      generate_destruction: 2,
                      generate_native_type: 1,
                      generate_arg_parse: 3,
                      ptr_level: 1

  defmacro __using__(_args) do
    quote do
      @behaviour unquote(__MODULE__)
      import Unifex.CodeGenerator.Utils, only: [sigil_g: 2]
    end
  end

  @doc """
  Provides a way to convert native variable `name` into `UNIFEX_TERM`

  Tries to get value from type-specific module, uses `enif_make_\#\{type}` as fallback value.
  """
  @spec generate_arg_serialize(t, name :: atom, CodeGenerator.t(), ctx :: map) ::
          CodeGenerator.code_t()
  def generate_arg_serialize(type, name, code_generator, ctx) do
    call(
      type,
      :generate_arg_serialize,
      [name],
      code_generator,
      ctx
    )
  end

  @doc """
  Generates a declaration of parameter (to be placed in function header) based on `c:generate_native_type/1` and
  provided `name`.

  Uses `type` as fallback for `c:generate_native_type/1`

  When mode is set to :const_unless_ptr_on_ptr, function will choose to behave like it would be set to :default or :const,
  depending on value returned by `ptr_level(type, code_generator, ctx)`.
  This mode can be used in places, when in general, you want to have declaration of variable with const type, but using :const
  mode would generate code, that would require explicit cast to avoid generating warnings during compilation - e.g. in C,
  passing argument of type `char **` to function, that expects argument of type `char const * const *` without any explicit cast,
  will generate such a warning
  """
  @spec generate_declaration(
          t,
          name :: atom,
          mode :: :default | :const | :const_unless_ptr_on_ptr,
          CodeGenerator.t(),
          ctx :: map
        ) ::
          [CodeGenerator.code_t()]
  def generate_declaration(type, name, mode \\ :default, code_generator, ctx)

  def generate_declaration(type, name, :const_unless_ptr_on_ptr, code_generator, ctx) do
    mode = if ptr_level(type, code_generator, ctx) < 2, do: :const, else: :default
    generate_declaration(type, name, mode, code_generator, ctx)
  end

  def generate_declaration(type, name, mode, code_generator, ctx) do
    generate_native_type(type, mode, code_generator, ctx)
    |> Bunch.listify()
    |> Enum.map(fn
      {type, suffix} -> ~g<#{type} #{name}#{suffix}>
      type -> ~g<#{type} #{name}>
    end)
  end

  @doc """
  Generates an initialization of variable content. Should be paired with `generate_destruction/1`

  Returns an empty string if the type does not provide initialization
  """
  @spec generate_initialization(t, name :: atom, CodeGenerator.t(), ctx :: map) ::
          CodeGenerator.code_t()
  def generate_initialization(type, name, code_generator, ctx) do
    call(type, :generate_initialization, [name], code_generator, ctx)
  end

  @doc """
  Generates an destrucition of variable content. Should be paired with `generate_initialization/1`

  Returns an empty string if the type does not provide destructor
  """
  @spec generate_destruction(t, name :: atom, CodeGenerator.t(), ctx :: map) ::
          CodeGenerator.code_t()
  def generate_destruction(type, name, code_generator, ctx) do
    call(type, :generate_destruction, [name], code_generator, ctx)
  end

  @doc """
  Generates parsing of UNIFEX_TERM `argument` into the native variable
  """
  @spec generate_arg_parse(
          t,
          name :: atom,
          argument :: CodeGenerator.code_t(),
          (CodeGenerator.code_t() -> CodeGenerator.code_t()),
          CodeGenerator.t(),
          map
        ) :: CodeGenerator.code_t()
  def generate_arg_parse(type, name, argument, postproc_fun \\ & &1, code_generator, ctx) do
    call(
      type,
      :generate_arg_parse,
      [argument, name],
      code_generator,
      Map.put(ctx, :postproc_fun, postproc_fun)
    )
    |> postproc_fun.()
  end

  @spec generate_arg_name(t, name :: atom, CodeGenerator.t(), map) :: [CodeGenerator.code_t()]
  def generate_arg_name(type, name, code_generator, ctx) do
    generate_native_type(type, code_generator, ctx)
    |> Bunch.listify()
    |> Enum.map(fn
      {_type, suffix} -> ~g<#{name}#{suffix}>
      _type -> ~g<#{name}>
    end)
  end

  @spec generate_native_type(t, :const | :default, CodeGenerator.t(), map) ::
          CodeGenerator.code_t()
  def generate_native_type(type, mode \\ :default, code_generator, ctx) do
    call(type, :generate_native_type, [], code_generator, Map.put(ctx, :mode, mode))
  end

  @spec ptr_level(t, CodeGenerator.t(), ctx :: map) :: integer
  def ptr_level(type, code_generator, ctx) do
    call(type, :ptr_level, [], code_generator, ctx)
  end

  defp call(full_type, callback, args, code_generator, ctx) do
    {type, subtype} =
      case full_type do
        {type, subtype} -> {type, subtype}
        type -> {type, nil}
      end

    ctx =
      with %{user_types: %{^type => type_spec}} <- ctx do
        Map.put(ctx, :type_spec, type_spec)
      else
        ctx -> ctx
      end

    module =
      with %{user_types: %{^type => %{__struct__: module}}} <- ctx do
        module
      else
        _ctx -> Module.concat(BaseTypes, type |> to_string() |> String.capitalize())
      end

    gen_aware_module = Module.concat(module, code_generator)

    default_gen_aware_module = Module.concat(BaseTypes.Default, code_generator)

    args =
      args ++ [Map.merge(ctx, %{generator: code_generator, type: full_type, subtype: subtype})]

    [gen_aware_module, module, default_gen_aware_module]
    |> Enum.find(
      BaseTypes.Default,
      &(Code.ensure_loaded?(&1) and function_exported?(&1, callback, length(args)))
    )
    |> apply(callback, args)
  end
end
