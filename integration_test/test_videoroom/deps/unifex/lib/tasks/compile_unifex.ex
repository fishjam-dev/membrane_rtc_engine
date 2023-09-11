defmodule Mix.Tasks.Compile.Unifex do
  @shortdoc "Generates native boilerplate code for all the `.spec.exs` files found in `c_src` dir"
  @moduledoc """
  #{@shortdoc}
  """

  use Mix.Task
  alias Unifex.{CodeGenerator, Helper, InterfaceIO, Specs}

  @recursive true

  @impl Mix.Task
  def run(_args) do
    {:ok, _apps} = Application.ensure_all_started(:unifex)

    Helper.get_source_dir()
    |> InterfaceIO.get_interfaces_specs!()
    |> Enum.each(fn {name, dir, specs_file} ->
      codes = specs_file |> Specs.parse(name) |> CodeGenerator.generate_code()
      Enum.each(codes, &InterfaceIO.store_interface!(name, dir, &1))
      generators = Enum.map(codes, fn {_header, _source, generator} -> generator end)
      tie_header = Unifex.CodeGenerator.TieHeader.generate_header(name, generators)
      InterfaceIO.store_tie_header!(name, dir, tie_header)
      InterfaceIO.store_gitignore!(dir)
    end)
  end
end
