defmodule Membrane.RTC.Engine.Signalling.Webrtc.ClientSignallingMsg do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :content, 0

  field :join, 1, type: :bytes, oneof: 0
  field :leave, 2, type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.Empty, oneof: 0
  field :updatePeerMetadata, 3, type: :bytes, oneof: 0

  field :updateTrackMetadata, 4,
    type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.TrackWithMetadata,
    oneof: 0

  field :renegotiateTracks, 5, type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.Empty, oneof: 0
  field :candidate, 9, type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.ICECandidate, oneof: 0
  field :sdpOffer, 10, type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.SdpOffer, oneof: 0

  field :setTargetVariant, 11,
    type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.TrackVariant,
    oneof: 0
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.ServerSignallingMsg do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :content, 0

  field :peerAccepted, 1, type: :string, oneof: 0
  field :peerJoined, 2, type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.Peer, oneof: 0
  field :peerLeft, 3, type: :string, oneof: 0
  field :trackAdded, 4, type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.Track, oneof: 0
  field :trackRemoved, 5, type: :string, oneof: 0

  field :variantSwitched, 6,
    type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.TrackVariant,
    oneof: 0

  field :sdpAnswer, 7, type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.SdpAnswer, oneof: 0
  field :offerData, 8, type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.OfferData, oneof: 0
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.Empty do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.TrackWithMetadata do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :trackId, 2, type: :string
  field :metadata, 3, type: :bytes
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.Track do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :owner, 1, type: :string
  field :trackId, 2, type: :string
  field :metadata, 3, type: :bytes
  field :mid, 4, type: :string
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.ICECandidate do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :candidate, 1, type: :string
  field :mLineIndex, 2, type: :uint64
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.SdpOffer do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :sdp, 1, type: :string
  field :tracks, 2, repeated: true, type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.Track
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.SdpAnswer.MidToTrackIdEntry do
  @moduledoc false
  use Protobuf, map: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.SdpAnswer do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :sdp, 1, type: :string

  field :midToTrackId, 2,
    repeated: true,
    type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.SdpAnswer.MidToTrackIdEntry
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.TrackVariant do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :trackId, 1, type: :string
  field :variant, 2, type: :string
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.TrackVariantSwitched do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :newVariant, 1, type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.TrackVariant
  field :reason, 2, type: :string
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.OfferData.TurnServer do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :addr, 1, type: :string
  field :port, 2, type: :uint32
  field :transport, 3, type: :string
  field :password, 4, type: :string
  field :username, 5, type: :string
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.OfferData do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :audioTracks, 1, type: :uint32
  field :videoTracks, 2, type: :uint32

  field :integratedTurnServers, 3,
    repeated: true,
    type: Membrane.RTC.Engine.Signalling.Webrtc.Payload.OfferData.TurnServer
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload.Peer do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :id, 1, type: :string
  field :metadata, 2, type: :bytes
end

defmodule Membrane.RTC.Engine.Signalling.Webrtc.Payload do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3
end