defmodule Membrane.RTC.Engine.Track do
  @moduledoc """
  Module representing media track.

  Media track is a single audio or video. Tracks that are related to each other
  (e.g. audio from microphone that corresponds to video from a web cam) can be grouped into the same stream by
  assigning each of them the same stream id.
  """
  use Bunch.Access

  alias ExSDP.Attribute.FMTP

  @enforce_keys [
    :type,
    :stream_id,
    :id,
    :origin,
    :fmtp,
    :encoding,
    :variants,
    :clock_rate,
    :active?,
    :metadata,
    :ctx
  ]
  defstruct @enforce_keys ++ [:payload_type]

  @supported_variants [:high, :medium, :low]

  @type id :: String.t()
  @type encoding :: atom()

  @typedoc """
  Possible track variants.

  The usage should be as follows:
  * `:high` - main track variant
  * `:medium` -  lower (in terms of quality) track variant
  * `:low` - the lowest track variant

  Audio tracks can have only one variant - `:high`.
  """
  @type variant :: :high | :medium | :low

  @typedoc """
  This module contains:
  * `type` - audio or video,
  * `stream_id` - media stream this track belongs to. Relationship between tracks (e.g. audio and video)
  can be indicated by assigning each of them the same `stream_id`. One `stream_id` can be assign to any
  number of tracks.
  * `id` - track id
  * `origin` - id of Endpoint this track belongs to
  * `encoding` - track encoding
  * `variants` - list of possible track variants. Refer to `t:variant/0`.
  * `clock_rate` - track clock rate
  * `fmtp` - struct describing format specific parameters e.g. for H264 it contains `profile_level_id`
  * `active?` - indicates whether track is still available or not (because client left a room)
  * `metadata` - any data passed by user to be linked with this track
  * `ctx` - any data Endpoints need to associate with `#{inspect(__MODULE__)}.t()` for internal usage
  """
  @type t :: %__MODULE__{
          type: :audio | :video,
          stream_id: String.t(),
          # TODO: remove pt and clock-rate and replace it with RTP mapping field
          payload_type: non_neg_integer() | nil,
          id: id,
          origin: String.t(),
          encoding: encoding,
          variants: [variant()],
          clock_rate: Membrane.RTP.clock_rate_t(),
          fmtp: FMTP.t(),
          active?: boolean(),
          metadata: any(),
          ctx: map()
        }

  @typedoc """
  Options that can be passed to `new/7`.

  If not provided:
  * `id` - will be generated
  * `active?` - true
  * `variants` - `[:high]`
  * `metadata` - `nil`
  * `ctx` - `%{}`

  For more information refer to `t:t/0`.
  """
  @type opts_t :: [
          id: String.t(),
          active?: boolean(),
          variants: [variant()],
          metadata: any(),
          ctx: map(),
          payload_type: non_neg_integer() | nil
        ]

  @doc """
  Creates a new track.

  Tracks belonging to the same stream should have the same `stream_id`,
  that can be generated with `stream_id/0`.
  """
  @spec new(
          :audio | :video,
          String.t(),
          String.t(),
          encoding(),
          Membrane.RTP.clock_rate_t(),
          FMTP.t(),
          opts_t()
        ) :: t
  def new(type, stream_id, origin, encoding, clock_rate, fmtp, opts \\ []) do
    id = Keyword.get(opts, :id, Base.encode16(:crypto.strong_rand_bytes(8)))

    %__MODULE__{
      type: type,
      payload_type: Keyword.get(opts, :payload_type),
      stream_id: stream_id,
      origin: origin,
      encoding: encoding,
      clock_rate: clock_rate,
      fmtp: fmtp,
      id: id,
      active?: Keyword.get(opts, :active?, true),
      variants: Keyword.get(opts, :variants, [:high]),
      metadata: Keyword.get(opts, :metadata),
      ctx: Keyword.get(opts, :ctx, %{})
    }
  end

  @doc """
  Generates stream id, that can be used to mark tracks belonging to the same stream.
  """
  @spec stream_id() :: String.t()
  def stream_id(), do: UUID.uuid4()

  @doc """
  Returns list of supported track variants.
  """
  @spec supported_variants() :: [variant()]
  def supported_variants(), do: @supported_variants

  @doc """
  Returns depayloader for given track.

  Depayloader can be used to unpack RTP stream i.e. get
  data out of RTP packets.

  Returns Membrane child specification or `nil` if depayloader
  couldn't be determined.

  ## Examples

      @impl true
      def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
        children = %{
          {:track_receiver, track_id} => %TrackReceiver{
            track: Map.fetch!(state.tracks, track_id),
            initial_target_variant: :high
          },
          {:depayloader, track.id} => get_depayloader(track),
          {:serializer, track_id} => Membrane.Stream.Serializer,
          {:sink, track_id} => %Membrane.File.Sink{
            location: Path.join(state.output_dir_path, track_id)
          }
        }

        links = [
          link_bin_input(pad)
          |> to({:track_receiver, track_id})
          |> to({:depayloader, track.id})
          |> to({:serializer, track_id})
          |> to({:sink, track_id})
        ]

        {{:ok, spec: %ParentSpec{children: children, links: links}}, state}
      end
  """
  @spec get_depayloader(t()) :: Membrane.ChildrenSpec.child_definition_t() | nil
  def get_depayloader(track) do
    case Membrane.RTP.PayloadFormat.get(track.encoding).depayloader do
      nil ->
        nil

      depayloader ->
        %Membrane.RTP.DepayloaderBin{
          depayloader: depayloader,
          clock_rate: track.clock_rate
        }
    end
  end

  @doc """
  Checks whether track is a simulcast one or not.
  """
  @spec is_simulcast?(t()) :: boolean()
  def is_simulcast?(%__MODULE__{} = track), do: track.variants != [:high]
end
