defmodule Unifex.CodeGenerator.TieHeader do
  @moduledoc false
  # Generates connective header file that includes proper header based
  # on selected interface.

  import Unifex.CodeGenerator.Utils, only: [sigil_g: 2]
  alias Unifex.CodeGenerator

  @spec generate_header(name :: Unifex.Specs.native_name_t(), generators :: [CodeGenerator.t()]) ::
          CodeGenerator.code_t()
  def generate_header(name, generators) do
    ~g"""
    #pragma once

    #{generate_includes(name, generators)}
    """
  end

  defp generate_includes(name, generators) do
    Enum.map_join(generators, "\n", &generate_include(name, &1))
  end

  defp generate_include(name, generator) do
    ~g"""
    #ifdef #{generator.identification_constant()}
    #include "#{generator.interface_io_name()}/#{name}.h"
    #endif
    """
  end
end
