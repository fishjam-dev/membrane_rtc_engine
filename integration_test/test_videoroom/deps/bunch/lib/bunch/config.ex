defmodule Bunch.Config do
  @moduledoc """
  A bunch of helpers for parsing and validating configurations.
  """

  use Bunch

  alias Bunch.Type

  @doc """
  Parses `config` according to `fields_specs`.

  `fields_specs` consist of constraints on each field. Supported constraints are:
    * validate - function determining if field's value is correct
    * in - enumerable containing all valid values
    * default - value returned if a field is not found in `config`
    * require? - determines whether a field is required, defaults to `false` when `default` is set
      and `true` when `default` is not set
    * require_if - deprecated, pass function returning constraints instead

  Instead of a list of constraints, a function accepting fields parsed so far and returning
  the constraints can be passed. If the function returns `nil`, field is considered non existent,
  as if it wasn't passed at all.

  ## Examples

      iex> #{inspect(__MODULE__)}.parse([a: 1, b: 2], a: [validate: & &1 > 0], b: [in: -2..2])
      {:ok, %{a: 1, b: 2}}
      iex> #{inspect(__MODULE__)}.parse([a: 1, b: 4], a: [validate: & &1 > 0], b: [in: -2..2])
      {:error, {:config_field, {:invalid_value, [key: :b, value: 4, reason: {:not_in, -2..2}]}}}
      iex> #{inspect(__MODULE__)}.parse(
      ...> [a: 1, b: 2],
      ...> a: [validate: & &1 > 0],
      ...> b: [in: -2..2],
      ...> c: [default: 5]
      ...> )
      {:ok, %{a: 1, b: 2, c: 5}}
      iex> #{inspect(__MODULE__)}.parse(
      ...> [a: 1, b: 2],
      ...> a: [validate: & &1 > 0],
      ...> b: [in: -2..2],
      ...> c: [require?: false]
      ...> )
      {:ok, %{a: 1, b: 2}}
      iex> #{inspect(__MODULE__)}.parse(
      ...> [a: 1, b: 1],
      ...> a: [validate: & &1 > 0],
      ...> b: [in: -2..2],
      ...> c: &(if &1.a == &1.b, do: [in: 0..1])
      ...> )
      {:error, {:config_field, {:key_not_found, :c}}}

  """
  @spec parse(
          config :: Keyword.t(v),
          [field | {field, field_specs | (parsed_config -> field_specs)}]
        ) :: Type.try_t(parsed_config)
        when parsed_config: %{atom => v},
             field: atom,
             v: any,
             field_specs:
               [
                 validate:
                   (v | any -> Type.try_t() | boolean)
                   | (v | any, parsed_config -> Type.try_t() | boolean),
                 in: Enumerable.t(),
                 default: v,
                 require?: boolean,
                 require_if: (parsed_config -> boolean)
               ]
               | nil
  def parse(config, fields_specs) do
    withl kw: true <- config |> Keyword.keyword?(),
          dup: [] <- config |> Keyword.keys() |> Bunch.Enum.duplicates(),
          do: config = config |> Map.new(),
          fields:
            {:ok, remaining_config, parsed_config} when remaining_config == %{} <-
              parse_fields(config, fields_specs, %{}) do
      {:ok, parsed_config}
    else
      kw: false ->
        {:error, {:config_not_keyword, config}}

      dup: duplicates ->
        {:error, {:config_duplicates, duplicates}}

      fields: {:error, reason} ->
        {:error, {:config_field, reason}}

      fields: {:ok, remaining_config, _parsed_config} ->
        {:error, {:config_invalid_keys, Map.keys(remaining_config)}}
    end
  end

  defp parse_fields(config, [], parsed_config) do
    {:ok, config, parsed_config}
  end

  defp parse_fields(config, [field_spec | fields_specs], parsed_config) do
    {key, spec} =
      case field_spec do
        key when is_atom(key) -> {key, []}
        {key, spec} when is_list(spec) -> {key, spec}
        {key, spec} when is_function(spec, 1) -> {key, spec.(parsed_config)}
      end

    if spec == nil do
      parse_fields(config, fields_specs, parsed_config)
    else
      case parse_field(key, Map.new(spec), Map.fetch(config, key), parsed_config) do
        {:ok, {key, value}} ->
          parse_fields(Map.delete(config, key), fields_specs, Map.put(parsed_config, key, value))

        {:ok, :ignore} ->
          parse_fields(Map.delete(config, key), fields_specs, parsed_config)

        {:error, :field_not_accepted} ->
          parse_fields(config, fields_specs, parsed_config)

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  defp parse_field(key, %{require_if: require_if} = spec, value, config) do
    require Logger

    Logger.warn(
      "Passing :require_if option to Bunch.Config.parse/2 is deprecated, pass function returning constraints instead"
    )

    spec = spec |> Map.delete(:require_if)

    cond do
      require_if.(config) ->
        parse_field(key, spec |> Map.delete(:default), value, config)

      Map.has_key?(spec, :default) ->
        parse_field(key, spec, value, config)

      true ->
        {:error, :field_not_accepted}
    end
  end

  defp parse_field(key, %{default: default}, :error, _config) do
    {:ok, {key, default}}
  end

  defp parse_field(_key, %{require?: false}, :error, _config) do
    {:ok, :ignore}
  end

  defp parse_field(key, _spec, :error, _config) do
    {:error, {:key_not_found, key}}
  end

  defp parse_field(key, spec, {:ok, value}, config) do
    validate = spec |> Map.get(:validate, fn _value -> :ok end)
    in_enum = spec |> Map.get(:in, [value])

    withl fun:
            res when res in [:ok, true] <-
              (case Function.info(validate)[:arity] do
                 1 -> validate.(value)
                 2 -> validate.(value, config)
               end),
          enum: true <- value in in_enum do
      {:ok, {key, value}}
    else
      fun: false ->
        {:error, {:invalid_value, key: key, value: value}}

      fun: {:error, reason} ->
        {:error, {:invalid_value, key: key, value: value, reason: reason}}

      enum: false ->
        {:error, {:invalid_value, key: key, value: value, reason: {:not_in, in_enum}}}
    end
  end
end
