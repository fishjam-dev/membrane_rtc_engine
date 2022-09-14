defmodule Membrane.RTC.Engine.Exception do
  @moduledoc false

  defmodule RequestTrackVariantError do
    defexception [:message]

    @impl true
    def exception(opts) do
      requester = Keyword.fetch!(opts, :requester)
      requested_variant = Keyword.fetch!(opts, :requested_variant)
      track = Keyword.fetch!(opts, :track)

      msg = """
      Endpoint: #{inspect(requester)} requested non-existing track variant: \
      #{inspect(requested_variant)} for track: #{inspect(track)}.
      """

      %__MODULE__{message: msg}
    end
  end

  defmodule TrackVariantStateError do
    defexception [:message]

    @impl true
    def exception(opts) do
      track = Keyword.fetch!(opts, :track)
      track_variant = Keyword.fetch!(opts, :variant)

      msg = """
      Endpoint: #{inspect(track.origin)} sent data for inactive track variant: #{inspect(track_variant)}. \
      Data can be sent only for active track variants. \
      To mark track variant as active send TrackVariantResumed event on your output pad.\
      """

      %__MODULE__{message: msg}
    end
  end
end
