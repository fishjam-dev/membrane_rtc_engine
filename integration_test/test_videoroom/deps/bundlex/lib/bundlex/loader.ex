defmodule Bundlex.Loader do
  @moduledoc """
  Some utilities to ease loading of Bundlex-based NIFs.
  """

  alias Bundlex.Helper.{MixHelper, PathHelper}

  @doc """
  Binds the module to the specified NIF.

  Accepts keyword list, that may contain two arguments:
  - `:nif` - required, name of the NIF to be bound (the same as specified in `bundlex.exs`)
  - `:app` - application defining the NIF, defaults to the current application

  After `use`'ing this module you can utilize `defnif/1` and `defnifp/1` macros
  to create bindings to particular native functions.

  ## Example

      defmodule My.Native.Module do
        use Bundlex.Loader, nif: :my_nif

        defnif native_function(arg1, arg2, arg3)

        def normal_function(arg1, arg2) do
          # ...
        end

        defnifp private_native_function(arg1, arg2)

      end
  """
  defmacro __using__(keyword) do
    quote do
      import unquote(__MODULE__), only: [defnif: 1, defnifp: 1]
      @before_compile unquote(__MODULE__)
      @bundlex_nif_name unquote(keyword |> Keyword.fetch!(:nif))
      @bundlex_app unquote(keyword |> Keyword.get(:app))
      Module.register_attribute(__MODULE__, :bundlex_defnifs, accumulate: true)
    end
  end

  @doc false
  @spec __before_compile__(Macro.Env.t()) :: :ok
  def __before_compile__(%{module: module} = env) do
    funs = Module.delete_attribute(module, :bundlex_defnifs)
    nif_name = Module.delete_attribute(module, :bundlex_nif_name)
    app = Module.delete_attribute(module, :bundlex_app)

    defs =
      funs
      |> Enum.map(fn fun ->
        {name, _location, args} = fun

        args =
          args
          |> Enum.map(fn
            {:\\, _meta, [arg, _default]} -> arg
            arg -> arg
          end)

        quote do
          # credo:disable-for-next-line Credo.Check.Readability.Specs
          def unquote(fun) do
            :erlang.nif_error(
              "Nif fail: #{unquote(module)}.#{unquote(name)}/#{length(unquote(args))}"
            )
          end
        end
      end)

    nif_module_content =
      quote do
        @moduledoc false
        require unquote(__MODULE__)
        @on_load :load_nif
        @spec load_nif() :: :ok | no_return()
        def load_nif() do
          unquote(__MODULE__).load_nif!(unquote(app), unquote(nif_name))
        end

        unquote(defs)
      end

    Module.create(module |> Module.concat(Nif), nif_module_content, env)
    :ok
  end

  @doc """
  Generates function bound to the native implementation. This module has to be
  `use`d for this macro to work.

  Function name should correspond to the native one.

  See `__using__/1` for examples.
  """
  defmacro defnif({name, _pos, args} = definition) do
    quote do
      @bundlex_defnifs unquote(Macro.escape(definition))
      @compile {:inline, [unquote({name, length(args)})]}
      defdelegate unquote(definition), to: __MODULE__.Nif
    end
  end

  @doc """
  Works the same way as `defnif/1`, but generates private function. This module
  has to be `use`d for this macro to work.

  See `__using__/1` for examples.
  """
  defmacro defnifp({name, _pos, args} = definition) do
    quote do
      @bundlex_defnifs unquote(Macro.escape(definition))
      @compile {:inline, [unquote({name, length(args)})]}
      defp unquote(definition) do
        __MODULE__.Nif.unquote(definition)
      end
    end
  end

  @doc """
  Binds calling module to NIF `nif_name` from application `app`.

  Second argument has to be an atom, the same as name of the NIF in the bundlex
  project.

  Invoked internally by `__using__/1` macro, which is the preferred way of loading
  NIFs.
  """
  defmacro load_nif!(app \\ nil, nif_name) do
    quote do
      app = unquote(app || MixHelper.get_app!())
      nif_name = unquote(nif_name)
      path = Bundlex.build_path(app, nif_name, :nif)

      with :ok <- :erlang.load_nif(path |> PathHelper.fix_slashes() |> to_charlist(), 0) do
        :ok
      else
        {:error, {reason, text}} ->
          raise """
          Bundlex cannot load nif #{inspect(nif_name)} of app #{inspect(app)}
          from "#{path}", check bundlex.exs file for information about nifs.
          Reason: #{inspect(reason)}, #{to_string(text)}
          """
      end
    end
  end
end
