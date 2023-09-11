defmodule Unifex.CodeGenerator.Utils do
  @moduledoc """
  Utilities for code generation.
  """
  use Bunch
  alias Unifex.CodeGenerator
  alias Unifex.CodeGenerator.BaseType

  @doc """
  Sigil used for templating generated code.
  """
  @spec sigil_g(String.t(), []) :: String.t()
  def sigil_g(content, flags) do
    if flags != [], do: raise("unsupported flags #{inspect(flags)}")
    content
  end

  @doc """
  Replaces special characters such as: `.`, `->` and array brackets e.g. `var_name[i]`
  with underscores.

  In case of arrays `var_name[i]` results in `var_name_i`.
  """
  @spec sanitize_var_name(String.t()) :: String.t()
  def sanitize_var_name(var_name) do
    var_name
    |> String.replace([".", "->"], "_")
    |> String.replace(~r/\[(.*)\]/, "_\\1")
  end

  @doc """
  Traverses Elixir specification AST and creates C data types serialization
  with `serializers`.
  """
  @spec generate_serialization(
          ast :: Macro.t(),
          serializers :: %{
            arg_serializer: (type :: BaseType.t(), name :: atom -> output),
            tuple_serializer: ([output] -> output)
          }
        ) ::
          {output, [{:label, atom} | {:arg, {name :: atom, type :: BaseType.t()}}]}
        when output: term

  def generate_serialization({:__aliases__, [alias: als], atoms}, serializers) do
    generate_serialization(als || Module.concat(atoms), serializers)
  end

  def generate_serialization(atom, serializers) when is_atom(atom) do
    {serializers.arg_serializer.(:atom, :"\"#{atom}\""), []}
  end

  def generate_serialization({:"::", _meta, [name, {:label, _meta2, _args}]}, serializers)
      when is_atom(name) do
    {serializers.arg_serializer.(:atom, :"\"#{name}\""), label: name}
  end

  def generate_serialization(
        {:"::", _meta, [{name, _meta2, _args}, {type, _meta3, _args2}]},
        serializers
      ) do
    {serializers.arg_serializer.(type, name), arg: {name, type}}
  end

  def generate_serialization(
        {:"::", meta, [name_var, [{type, type_meta, type_ctx}]]},
        serializers
      ) do
    generate_serialization(
      {:"::", meta, [name_var, {{:list, type}, type_meta, type_ctx}]},
      serializers
    )
  end

  def generate_serialization({a, b}, serializers) do
    generate_serialization({:{}, [], [a, b]}, serializers)
  end

  def generate_serialization({:{}, _meta, content}, serializers) do
    {results, meta} =
      content
      |> Enum.map(fn ast -> generate_serialization(ast, serializers) end)
      |> Enum.unzip()

    {serializers.tuple_serializer.(results), List.flatten(meta)}
  end

  def generate_serialization([{_name, _meta, _args} = name_var], serializers) do
    generate_serialization(
      {:"::", [], [name_var, [name_var]]},
      serializers
    )
  end

  def generate_serialization({_name, _meta, _args} = name_var, serializers) do
    generate_serialization({:"::", [], [name_var, name_var]}, serializers)
  end

  @spec generate_functions(
          config :: Enumerable.t(),
          generator :: (term, map -> CodeGenerator.code_t()),
          ctx :: map
        ) :: CodeGenerator.code_t()
  def generate_functions(config, generator, ctx) do
    do_generate(config, generator, ctx)
  end

  @spec generate_functions_declarations(
          config :: Enumerable.t(),
          generator :: (term, map -> CodeGenerator.code_t()),
          ctx :: map
        ) :: CodeGenerator.code_t()
  def generate_functions_declarations(config, generator, ctx) do
    do_generate(config, generator, &(&1 <> ";"), ctx)
  end

  @spec generate_structs_definitions(
          config :: Enumerable.t(),
          generator :: (term, map -> CodeGenerator.code_t()),
          ctx :: map
        ) :: CodeGenerator.code_t()
  def generate_structs_definitions(config, generator, ctx) do
    do_generate(config, generator, ctx)
  end

  @spec generate_enums_definitions(
          config :: Enumerable.t(),
          generator :: (term, map -> CodeGenerator.code_t()),
          ctx :: map
        ) :: CodeGenerator.code_t()
  def generate_enums_definitions(config, generator, ctx) do
    do_generate(config, generator, ctx)
  end

  @spec generate_maybe_unused_args_statements(args :: [String.t()]) :: [String.t()]
  def generate_maybe_unused_args_statements(args) do
    args |> Enum.map(fn arg -> ~g<UNIFEX_MAYBE_UNUSED(#{arg});> end)
  end

  defp do_generate(config, generator, mapper \\ & &1, ctx) do
    config
    |> Enum.map(fn c -> generator.(c, ctx) end)
    |> Enum.filter(&(&1 != ""))
    |> Enum.map_join("\n", mapper)
  end
end
