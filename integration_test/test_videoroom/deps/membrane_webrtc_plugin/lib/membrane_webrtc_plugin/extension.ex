defmodule Membrane.WebRTC.Extension do
  @moduledoc """
  A module that provides mappings between `ExSDP.Attribute.Extmap` and modules implementing
  `Membrane.WebRTC.Extension` behaviour.
  """
  alias ExSDP.Attribute.Extmap
  alias ExSDP.Media
  alias Membrane.{RTP, WebRTC}

  @enforce_keys [:module, :uri, :name]
  defstruct @enforce_keys ++ [rtp_opts: Keyword.new()]

  @typedoc """
    * module - extension specific module which implement `Membrane.WebRTC.Extension` behaviour
    * rtp_opts - options for RTP module. This allows configuring RTP module to your needs
    * uri - a URI that identifies extension in SDP
    * name - an atom identifying the extension in `Membrane.RTP.SessionBin`
  """
  @type t :: %__MODULE__{
          module: module(),
          rtp_opts: Keyword.t(),
          uri: String.t(),
          name: atom()
        }
  @type maybe_t :: t() | :not_supported

  @doc """
  Creates a `#{inspect(__MODULE__)}` struct for a specific extension.
  """
  @callback new(opts :: Keyword.t()) :: t()

  @doc """
  Returns a boolean indicating whether an extension is compatible with given encoding.
  """
  @callback compatible?(WebRTC.Track.encoding_key()) :: boolean()

  @doc """
  Returns a module that implements the extension in `Membrane.RTP.SessionBin` or `:no_rtp_module` if such
  such module would only forward buffers to next element.

  `:inbound`/`:outbound` denotes track type this extension was specified for.
  """
  @callback get_rtp_module(Extmap.extension_id(), Keyword.t(), :inbound | :outbound) ::
              Membrane.ChildrenSpec.child_definition_t() | :no_rtp_module

  @doc """
  Adds information about extension to an SDP media.
  """
  @callback add_to_media(
              Media.t(),
              Extmap.extension_id(),
              Extmap.direction(),
              [RTP.payload_type_t()]
            ) ::
              Media.t()

  @doc """
  Returns an URI that identifies extension in SDP
  """
  @callback uri() :: String.t()

  @doc """
  Given a list of supported extensions, checks if there is an extension that corresponds to
  given `Extmap` and encoding.
  """
  @spec supported?([t()], Extmap.t(), atom()) :: boolean()
  def supported?(extensions, %Extmap{uri: uri}, encoding),
    do: Enum.any?(extensions, &(&1.uri == uri and &1.module.compatible?(encoding)))

  @doc """
  Given a list of supported extensions, returns an extension that corresponds to given `Extmap`
  or `:not_supported` if there is no such extension.
  """
  @spec from_extmap([t()], Extmap.t()) :: maybe_t()
  def from_extmap(extensions, %Extmap{uri: uri}),
    do: Enum.find(extensions, :not_supported, &(&1.uri == uri))

  @doc """
  Given an SDP media, a list of supported extensions and supported `Extmap`s, adds corresponding
  extensions to the media.
  """
  @spec add_to_media(Media.t(), [t()], [Extmap.t()], Extmap.direction(), [RTP.payload_type_t()]) ::
          Media.t()
  def add_to_media(media, _extensions, [], _direction, _pt), do: media

  def add_to_media(media, extensions, [extmap | rest], direction, payload_types) do
    extension = from_extmap(extensions, extmap)

    media
    |> extension.module.add_to_media(extmap.id, direction, payload_types)
    |> add_to_media(extensions, rest, direction, payload_types)
  end

  @doc """
  Given a list of supported extensions, maps a supported `Extmap` to an `RTP.SessionBin.rtp_extension_t()`.

  `:inbound`/`:outbound` denotes track type `extmap` was specified for.
  """
  @spec as_rtp_extension([t()], Extmap.t(), :inbound | :outbound) ::
          RTP.SessionBin.rtp_extension_option_t()
  def as_rtp_extension(extensions, extmap, track_type) do
    extension = from_extmap(extensions, extmap)
    {extension.name, extension.module.get_rtp_module(extmap.id, extension.rtp_opts, track_type)}
  end

  @doc """
  Given a list of supported extensions and a supported `Extmap`, generates a mapping from a name provided
  by an extension to an ID provided by the `Extmap`.
  """
  @spec as_rtp_mapping([t()], Extmap.t()) ::
          {RTP.SessionBin.rtp_extension_name_t(), Extmap.extension_id()}
  def as_rtp_mapping(extensions, extmap) do
    extension = from_extmap(extensions, extmap)
    {extension.name, extmap.id}
  end
end
