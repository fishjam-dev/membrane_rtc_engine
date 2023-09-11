defmodule Unifex.Specs do
  @moduledoc """
  Module that handles parsing Unifex specs for native boilerplate code generation.

  For information on how to create such specs, see `Unifex.Specs.DSL` module.
  """

  require Logger

  alias Unifex.CodeGenerator

  @typedoc """
  Name of interface generated for the native library.
  Must be a suffix of code generator module in `Unifex.CodeGenerators` namespace.

  Unifex has built-in generators for `NIF` and `CNode` interfaces.
  """
  @type interface_t :: atom()

  @type native_name_t :: atom()

  @type struct_t ::
          {struct_alias :: atom, struct_module_name :: atom,
           [{field_name :: atom, field_type :: {atom | {:list, atom}}}]}

  @type enum_t :: {enum_name :: atom, [enum_field :: atom]}

  @type t :: %__MODULE__{
          name: native_name_t,
          module: CodeGenerator.t() | nil,
          functions_args: [{function_name :: atom, [arg_type :: {atom | {:list, atom}}]}],
          functions_results: [{function_name :: atom, return_type :: Macro.t()}],
          functions_docs: [{function_name :: atom, documentation :: String.t() | false}],
          sends: [{send_name :: atom, send_term_type :: Macro.t()}],
          dirty_functions: %{
            {function_name :: atom, function_arity :: non_neg_integer} => :cpu | :io
          },
          callbacks: %{
            (hook :: :load | :upgrade | :unload | :main_function) => function_name :: String.t()
          },
          interface: [interface_t()] | interface_t() | nil,
          state_type: String.t() | nil,
          enums: [],
          structs: [struct_t()]
        }

  @enforce_keys [
    :name,
    :module,
    :functions_args,
    :functions_results,
    :functions_docs,
    :sends,
    :dirty_functions,
    :callbacks,
    :interface,
    :state_type,
    :enums,
    :structs
  ]

  defstruct @enforce_keys

  @doc """
  Parses Unifex specs of native functions.
  """
  @spec parse(specs_file :: String.t(), native_name_t) :: t()
  def parse(specs_file, name) do
    specs_code = File.read!(specs_file)
    {_res, binds} = Code.eval_string(specs_code, [{:unifex_config__, []}], make_env(specs_file))
    config = binds |> Keyword.fetch!(:unifex_config__) |> Enum.reverse()

    {functions_args, functions_results} =
      config
      |> Keyword.get_values(:function)
      |> Enum.map(fn {name, args, results} -> {{name, args}, {name, results}} end)
      |> Enum.unzip()

    functions_results =
      Enum.flat_map(functions_results, fn {name, results} -> Enum.map(results, &{name, &1}) end)

    functions_docs = parse_docs(config, specs_file)

    %__MODULE__{
      name: name,
      module: Keyword.get(config, :module),
      functions_args: functions_args,
      functions_results: functions_results,
      functions_docs: functions_docs,
      sends: Keyword.get_values(config, :sends),
      dirty_functions:
        config |> Keyword.get_values(:dirty_functions) |> List.flatten() |> Map.new(),
      callbacks: config |> Keyword.get_values(:callback) |> Map.new(),
      interface: Keyword.get(config, :interface),
      state_type: Keyword.get(config, :state_type, nil),
      enums: Keyword.get_values(config, :enum),
      structs: Keyword.get_values(config, :struct)
    }
  end

  defp parse_docs(config, specs_file) do
    config
    |> Enum.chunk_every(2, 1)
    |> Enum.flat_map(fn
      [doc: {_meta, doc}, function: function] ->
        [{function, doc}]

      [{:doc, {meta, _doc}}, _not_a_function] ->
        line_number = meta[:line]

        Logger.warning(
          "Found @doc in file #{specs_file}:#{line_number} that does not correspond to any function."
        )

        []

      [_prev_term, function: function] ->
        [{function, false}]

      _else ->
        []
    end)
    |> Keyword.new(fn {{name, _args, _results}, doc} -> {name, doc} end)
  end

  # Returns a clean __ENV__ with proper functions/macros imported. Useful for invoking
  # user code without possibly misleading macros and aliases from the current scope,
  # while providing needed functions/macros.
  defp make_env(file) do
    {env, _binds} =
      Code.eval_quoted(
        quote do
          import Kernel, except: [@: 1]
          import Unifex.Specs.DSL
          %Macro.Env{__ENV__ | file: unquote(file)}
        end
      )

    env
  end
end
