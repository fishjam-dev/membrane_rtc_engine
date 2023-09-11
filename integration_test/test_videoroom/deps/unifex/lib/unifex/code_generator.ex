defmodule Unifex.CodeGenerator do
  @moduledoc """
  Behaviour for code generation.
  """

  alias Unifex.Specs

  @type t :: module
  @type code_t :: String.t()
  @type generated_code_t :: {header :: code_t, source :: code_t, generator :: t}

  @callback identification_constant() :: String.t()
  @callback interface_io_name() :: String.t()
  @callback generate_header(specs :: Specs.t()) :: code_t
  @callback generate_source(specs :: Specs.t()) :: code_t

  @doc """
  Generates boilerplate code using generator implementation from `Unifex.CodeGenerators`.
  """
  @spec generate_code(Specs.t()) :: [generated_code_t()]
  def generate_code(specs) do
    for generator <- get_generators(specs) do
      header = generator.generate_header(specs)
      source = generator.generate_source(specs)
      {header, source, generator}
    end
  end

  @spec get_generators(Specs.t()) :: [t]
  defp get_generators(%Specs{name: name, interface: nil}) do
    {:ok, bundlex_project} = Bundlex.Project.get()
    config = bundlex_project.config

    generators =
      [:natives, :libs]
      |> Enum.flat_map(&Keyword.get(config, &1))
      |> Keyword.get_values(name)
      |> Enum.flat_map(&Bunch.listify(Keyword.get(&1, :interface)))
      |> Enum.map(&bundlex_interface/1)
      |> Enum.map(&interface_generator/1)

    case generators do
      [] -> raise "Interface for native #{name} is not specified.
        Please specify it in your *.spec.exs or bundlex.exs file."
      generators -> generators
    end
  end

  defp get_generators(%Specs{interface: interfaces}) do
    interfaces
    |> Bunch.listify()
    |> Enum.map(&interface_generator/1)
  end

  @spec bundlex_interface(Bundlex.Native.interface_t()) :: Specs.interface_t()
  def bundlex_interface(:cnode), do: CNode
  def bundlex_interface(:nif), do: NIF
  def bundlex_interface(:port), do: Port

  @spec interface_generator(Specs.interface_t()) :: t
  def interface_generator(interface) do
    generator = Module.concat(Unifex.CodeGenerators, interface)

    # Ensure CodeGenerator module is present
    true = Code.ensure_loaded?(generator)

    generator
  end
end
