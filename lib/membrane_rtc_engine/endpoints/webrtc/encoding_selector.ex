defmodule Membrane.RTC.Engine.Endpoint.WebRTC.EncodingSelector do
  @moduledoc false
  # module responsible for choosing track encoding

  require Membrane.Logger

  alias Membrane.RTC.Engine.Track

  @type t() :: %__MODULE__{
          target_encoding: Track.variant() | nil,
          current_encoding: Track.variant() | nil,
          queued_encoding: Track.variant() | nil,
          active_encodings: MapSet.t(Track.variant()),
          all_encodings: MapSet.t(Track.variant())
        }

  @enforce_keys [:all_encodings]
  defstruct @enforce_keys ++
              [
                :target_encoding,
                :current_encoding,
                :queued_encoding,
                active_encodings: MapSet.new()
              ]

  @doc """
  Creates new encoding selector.

  * `encodings` - a list of all track encodings. Encoding selector
  assumes that initialy all encodings are inactive. To mark encoding
  as active use `encoding_active/2`.
  * `initial_target_encoding` - encoding to prioritize. It will be
  chosen whenever it is active. Can be changed with `target_encoding/2`.
  """
  @spec new([Track.variant()], Track.variant() | nil) :: t()
  def new(encodings, initial_target_encoding \\ nil) do
    %__MODULE__{all_encodings: MapSet.new(encodings), target_encoding: initial_target_encoding}
  end

  @doc """
  Marks given `encoding` as inactive.

  Returns new selector and encoding to request
  or `nil` if there are no changes needed.
  """
  @spec encoding_inactive(t(), Track.variant()) :: {t(), Track.variant() | nil}
  def encoding_inactive(selector, encoding) do
    selector = %__MODULE__{
      selector
      | active_encodings: MapSet.delete(selector.active_encodings, encoding)
    }

    case selector do
      %{current_encoding: ^encoding} ->
        selector = %__MODULE__{selector | current_encoding: nil}
        select_encoding(selector)

      %{queued_encoding: ^encoding, current_encoding: nil} ->
        selector = %__MODULE__{selector | queued_encoding: nil}
        select_encoding(selector)

      %{queued_encoding: ^encoding} ->
        selector = %__MODULE__{selector | queued_encoding: nil}
        {selector, nil}

      _else ->
        {selector, nil}
    end
  end

  @doc """
  Marks given `encoding` as active.

  Returns new selector and encoding to request
  or `nil` if there are no changes needed.
  """
  @spec encoding_active(t(), Track.variant()) :: {t(), Track.variant() | nil}
  def encoding_active(selector, encoding) do
    selector = %__MODULE__{
      selector
      | active_encodings: MapSet.put(selector.active_encodings, encoding)
    }

    cond do
      selector.current_encoding == selector.target_encoding ->
        {selector, nil}

      selector.queued_encoding == selector.target_encoding ->
        {selector, nil}

      selector.target_encoding == encoding ->
        select_encoding(selector, encoding)

      true ->
        select_encoding(selector)
    end
  end

  @doc """
  Sets currently used encoding.

  Should be called when encoding change happens
  i.e. after receiving `Membrane.RTC.Engine.Event.TrackVariantSwitched` event.
  """
  @spec set_current_encoding(t(), Track.variant()) :: t()
  def set_current_encoding(%__MODULE__{queued_encoding: encoding} = selector, encoding) do
    %__MODULE__{selector | current_encoding: encoding, queued_encoding: nil}
  end

  def set_current_encoding(%__MODULE__{} = selector, encoding) do
    %__MODULE__{selector | current_encoding: encoding}
  end

  @doc """
  Sets the target encoding that should be selected whenever it is active.

  If the target encoding is not active, we will switch to it when it becomes active again
  """
  @spec set_target_encoding(t(), Track.variant()) :: {t(), Track.variant() | nil}
  def set_target_encoding(selector, encoding) do
    cond do
      encoding not in selector.all_encodings ->
        Membrane.Logger.warn("""
        Requested non existing encoding #{inspect(encoding)}. \
        Available track encodings: #{inspect(selector.all_encodings)}. \
        Ignoring.\
        """)

        {selector, nil}

      encoding in selector.active_encodings ->
        selector = %__MODULE__{selector | target_encoding: encoding}
        select_encoding(selector, encoding)

      true ->
        Membrane.Logger.debug("""
        Requested inactive encoding #{inspect(encoding)}. Saving it as target.
        It will be requested once it becomes active
        """)

        selector = %__MODULE__{selector | target_encoding: encoding}

        {selector, nil}
    end
  end

  defp select_encoding(%__MODULE__{active_encodings: encodings} = selector) do
    encoding = best_active_encoding(encodings)
    select_encoding(selector, encoding)
  end

  defp select_encoding(selector, nil) do
    Membrane.Logger.debug("No active encoding.")
    selector = %__MODULE__{selector | current_encoding: nil, queued_encoding: nil}
    {selector, nil}
  end

  defp select_encoding(
         %__MODULE__{current_encoding: encoding, queued_encoding: nil} = selector,
         encoding
       ) do
    Membrane.Logger.debug("Requested currently used encoding #{encoding}. Ignoring.")
    {selector, nil}
  end

  defp select_encoding(%__MODULE__{current_encoding: encoding} = selector, encoding) do
    Membrane.Logger.debug("""
    Requested encoding: #{inspect(encoding)} which is currently used but while waiting
    for keyframe for queued_encoding #{inspect(selector.queued_encoding)}.
    Clearing queued_encoding #{inspect(selector.queued_encoding)}
    """)

    selector = %__MODULE__{selector | queued_encoding: nil}
    {selector, nil}
  end

  defp select_encoding(selector, encoding) do
    Membrane.Logger.debug("Enqueuing encoding #{inspect(encoding)}.")
    selector = %__MODULE__{selector | queued_encoding: encoding}
    {selector, encoding}
  end

  defp best_active_encoding(encodings) do
    encodings |> sort_encodings() |> List.first()
  end

  defp sort_encodings(encodings) do
    Enum.sort_by(
      encodings,
      fn
        "h" -> 3
        "m" -> 2
        "l" -> 1
      end,
      :desc
    )
  end
end
