defmodule Membrane.RTC.Engine.Exception do
  @moduledoc false

  defmodule PublishTrackError do
    defexception [:message]

    @impl true
    def exception(opts) do
      track = Keyword.fetch!(opts, :track)

      msg = """
      Tried to publish track with invalid variants. For video tracks variants
      has to be a list with at least one variant of type Track.variant().
      Audio tracks can have just one variant (:high), therefore for audio tracks
      variants has to be a list in form of [:high].
      Track: #{inspect(track)}.
      """

      %__MODULE__{message: msg}
    end
  end

  defmodule TrackReadyError do
    defexception [:message]

    @impl true
    def exception(opts) do
      track = Keyword.fetch!(opts, :track)
      variant = Keyword.fetch!(opts, :variant)

      msg = """
      Tried to mark track variant: #{inspect(variant)} as ready but given
      track hasn't been published with such variant.
      Track: #{inspect(track)}.
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

  defmodule VoiceActivityError do
    defexception [:message]

    @impl true
    def exception(opts) do
      track = Keyword.fetch!(opts, :track)

      msg = """
      Endpoint: #{inspect(track.origin)} sent VoiceActivityChanged event for video track #{inspect(track.id)}. \
      VoiceActivityChanged can only be sent for audio tracks.
      """

      %__MODULE__{message: msg}
    end
  end
end
