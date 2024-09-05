# WebRTC Media Events

This document described all messages exchanged between the WebRTC Endpoint and a client.
In general, each media event has to be sent in the form:

```json
{
  type: name,
  data: data
}
```

where the name of the event is the value of the `type` field, while `data` should carry the data shown below for every media event

## Table of contents

Below you can find a summary of all media events defined inside `membrane_rtc_engine`

### Generic WebRTC Endpoint messages

Messages used by any WebRTC Endpoint plugin

#### WebRTC Endpoint receives these types of `media_events` from client:
| Name                                              | Description                                                     |
| ------------------------------------------------- | --------------------------------------------------------------- |
| [connect](#connect)                               | sent when client wants to connect to associated WebRTC Endpoint |
| [disconnect](#disconnect)                         | sent when client disconnects from associated endpoint           |
| [updateEndpointMetadata](#updateendpointmetadata) | contains new metadata of client's endpoint                      |
| [updateTrackMetadata](#updatetrackmetadata)       | contains new metadata of a track                                |
| [disableTrackEncoding](#disabletrackencoding)     | sent when client disables one of the track variants             |
| [enableTrackEncoding](#enabletrackencoding)       | sent when client enables one of the track variants              |
| [muteTrack](#mutetrack)                           | sent when track is muted                                        |
| [unmuteTrack](#unmutetrack)                       | sent when track is unmuted                                      |
| [custom](#custom)                                 | message forwarded to the endpoint associated with the client    |



#### WebRTC Endpoint sends these types of messages to the client:

| Name                                            | Description                                                                  |
| ----------------------------------------------- | ---------------------------------------------------------------------------- |
| [tracksPriority](#trackspriority)               | contains all tracks that will be forwarded to client until next this message |
| [endpointUpdated](#endpointupdated)             | contains new metadata of an endpoint                                         |
| [trackUpdated](#trackupdated)                   | contains new metadata of a track                                             |
| [tracksAdded](#tracksadded)                     | sent when some tracks were added by an endpoint                              |
| [tracksRemoved](#tracksremoved)                 | sent when some tracks were removed by an endpoint                            |
| [trackEncodingDisabled](#trackencodingdisabled) | sent when some track variant were disabled by an endpoint                    |
| [trackEncodingEnabled](#trackencodingenabled)   | sent when some track variant were enabled by an endpoint                     |
| [endpointAdded](#endpointadded)                 | sent after after new endpoint was added                                      |
| [connected](#connected)                         | sent as a response to [connect](#connect) when succesful                     |
| [endpointRemoved](#endpointremoved)             | sent after some endpoint was removed                                         |
| [error](#error)                                 | Notifies about an error that occurred                                        |
| [custom](#custom-1)                             | custom message forwarded from endpoint to client                             |

### WebRTC custom media events

#### WebRTC endpoint receives these types of custom `media_event`s from client:

| Name                                            | Description                                                               |
| ----------------------------------------------- | ------------------------------------------------------------------------- |
| [renegotiateTracks](#renegotiatetracks)         | A request from a client to renegotiate tracks                             |
| [prioritizeTrack](#prioritizetrack)             | A request to prioritize the track                                         |
| [unprioritizeTrack](#unprioritizetrack)         | A request to unprioritize the track                                       |
| [preferedVideoSizes](#preferedvideosizes)       | informs of how many videos in different quality a client wants to receive |
| [candidate](#candidate)                         | Contains client's ICE candidate                                           |
| [sdpOffer](#sdpoffer)                           | Contains an SDP offer from a client                                       |
| [setTargetTrackVariant](#setTargetTrackVariant) | A request from a client to receive a specific track variant               |
| [trackVariantBitrates](#trackVariantBitrate)    | Contains updated bitrates for track's variants                            |

#### WebRTC endpoint sends these type of custom messages to client
| Name                                  | Description                                                       |
| ------------------------------------- | ----------------------------------------------------------------- |
| [offerData](#offerdata)               | Sends data needed by the client to create an offer                |
| [candidate](#candidate-1)             | Contains an ICE candidate                                         |
| [sdpAnswer](#sdpanswer)               | Provides an SDP Answer to the client's offer                      |
| [encodingSwitched](#encodingswitched) | An information that a track will be sent with a specific encoding |
| [vadNotification](#vadnotification)   | An update on Voice Activity Detection                             |


## Client -> WebRTC Endpoint

### `connect`

* Sent when a client wants to join WebRTC Endpoint. It contains only endpoint's metadata

  ```json
  {
    "metadata": any
  }
  ```

### `disconnect`

* Sent when client disconnects from WebRTC Endpoint. It contains no data

  ```json
  {}
  ```

### `updateEndpointMetadata`

* Contains new metadata of client's endpoint

  ```json
  {
    "metadata": any
  }
  ```

### `disableTrackEncoding`

* Sent when client disables one of the track variants

  ```json
  {
    "trackId": track_id,
    "encoding": encoding
  }
  ```

### `enableTrackEncoding`

* Sent when client enables one of the track variants

  ```json
  {
    "trackId": track_id,
    "encoding": encoding
  }
  ```

### `updateTrackMetadata`

* Contains new metadata of some track and an id of this track

  ```json
  {
    "trackId": track_id,
    "trackMetadata": any
  }
  ```

### `muteTrack`

* Sent when a track is muted

  ```json
  {
    "trackId": track_id,
  }
  ```


### `unmuteTrack`

* Sent when a track is unmuted

  ```json
  {
    "trackId": track_id,
  }
  ```


### `custom`

* A black-box for a message that WebRTC Endpoint will forward to the endpoint associated with the client who sent the message.

  ```json
  any
  ```

## WebRTC Endpoint -> Client

### `tracksPriority`

* Contains all tracks that will be forwarded to client until the next `tracks_priority` message.
  This type of messages is sent only if DisplayManager is spawned

  ```json
  {
    tracks: tracks
  }
  ```

### `endpointUpdated`

* Contains information about updated metadata of one of the endpoints

  ```json
  {
    endpointId: endpoint_id,
    metadata: any
  }
  ```

### `trackUpdated`

* Contains information about new metadata of one of tracks

  ```json
  {
    endpointId: endpoint_id,
    trackId: track_id,
    metadata: any
  }
  ```

### `tracksAdded`

* Informs that one of the clients has added one or more tracks.
  It contains:
  - an id of endpoint associated with that client, 
  - a map of all tracks with `track_id`'s as keys and objects with `track_metadata` and simulcast conifg as a value 
  - (Depracated field use tracks) a map of all tracks with `track_id`s as keys and `track_metadata` as value.

  ```json
  {
    endpointId: endpoint_id,
    trackIdToMetadata: {
      track_id: any
    },
    tracks: {
      track_id: {
        metadata: any
        simulcastConfig: {
          enabled: boolean,
          activeEncodings: string[],
          disabledEncodings: string[]
        }
      }
    }
  }
  ```

### `tracksRemoved`

* Contains a list of tracks which have been removed by some client and id of that client's endpoint

  ```json
  {
    endpointId: endpoint_id,
    trackIds: track_ids
  }
  ```

### `trackEncodingDisabled`

* Sent when some track variant were disabled by an endpoint

  ```json
  {
    "trackId": track_id,
    "encoding": encoding
  }
  ```

### `trackEncodingEnabled`

* Sent when some track variant were enabled by an endpoint

  ```json
  {
    "trackId": track_id,
    "encoding": encoding
  }
  ```

### `endpointAdded`

* Message sent to all clients in the room after a new endpoint was added.
  It contains id and metadata of the new endpoint.

  ```json
  {
    id: endpoint_id,
    metadata: any
  }
  ```

### `connected`

* Message sent to the client after connecting to the WebRTC Endpoint. It contains the id of that client's endpoint and a list of information about endpoints in the Engine
(id, metadata, a `trackIdToMetadata` and tracks like seen in `tracksAdded`)

  ```json
  {
    id: endpoint_id,
    otherEndpoints: endpoints
  }
  ```

### `endpointRemoved`

* Sent to all remaining clients in the room after some endpoint was removed. It contains an id of the removed endpoint.

  ```json
  {
    id: endpoint_id
  }
  ```

### `error`

* Informs that an error occurred on the server providing a message to show

  ```json
  {
    message: any()
  }
  ```

### `custom`

* WARNING!! This type of media event is deprecated - will soon be removed.
* A black-box for a message that the WebRTC Endpoint will forward to the client.

  ```json
  any
  ```

## Client -> WebRTC Enpoint

### `renegotiateTracks`

* Informs that a client wants to renegotiate connection due to adding a track or removing a track

  ```json
  {}
  ```

### `prioritizeTrack`

* Contains an id of the track that the client wants to prioritize

  ```json
  {
    trackId: trackId
  }
  ```

### `unprioritizeTrack`

* Contains an id of the track that the client want to unprioritize.

  ```json
  {
    trackId: trackId
  }
  ```

### `preferedVideoSizes`

* Contains information of how many videos in different quality (high, medium, small) the client would like to receive.
  `allSameSize` flag which indicates that all videos should be in the same quality.

  ```json
  {
    bigScreens: Int,
    mediumScreens: Int,
    smallScreens: Int,
    allSameSize: boolean
  }
  ```

### `candidate`

* Contains an ICE candidate that is forwarded to the WebRTC Endpoint

  ```json
  {
    candidate: candidate,
    sdpMLineIndex: Int
  }
  ```

### `sdpOffer`

* Contains an SDP offer, a mapping between `track_id` and `track_metadata`, mapping between `track_id`
  and this track's bitrates (or all of its variants bitrates) and a mapping between `mid` and `track_id`.
  Maps contain only information about current client's `sendonly` tracks.

  ```json
  {
    sdpOffer: {
      type: "offer",
      sdp: sdp_offer
    },
    trackIdToTrackMetadata: {
      trackId: any
    },
    trackIdToTrackBitrates: {
      trackId: number | {rid: number}
    },
    midToTrackId: mid_to_track_id
  }
  ```

### `setTargetTrackVariant`

* Informs that a client wants to receive a specific track variant.
  The track variant will be sent whenever it is available.
  If choosen variant is unavailable, some other variant will be
  sent until choosen variant becomes active again.

  ```json
  {
    trackId => track_id,
    variant => variant
  }
  ```

### `trackVariantBitrates`

* Contains updated bitrates of variants of the track send by the client.
  Needs to contain all of the variants.

```json
{
  trackId: track_id,
  variantBitrates: {rid: number}
}
```

## WebRTC Enpoint -> Client

### `offerData`

* Contains information about the number  of audio and video tracks that will be sent from the engine
  to the client and information regarding the integrated TURN server.

  ```json
  {
    tracksTypes: {
      audio: Int,
      video: Int
    },
    integratedTurnServers: [{
      serverAddr: addr,
      serverPort: Int,
      transport: string,
      password: string,
      username: string
    }]
  }
  ```

### `candidate`

* Contains information about an ICE candidate which will be sent to the client

  ```json
  {
    candidate: candidate,
    sdpMLineIndex: sdp_m_line_index,
    sdpMid: nil,
    usernameFragment: nil
  }
  ```

### `sdpAnswer`

* Contains an SDP answer and mapping between `mid` and `track_id` for all tracks (active, inactive, inbound and outbound)

  ```json
  {
    type: "answer",
    sdp: sdp_answer,
    midToTrackId: mid_to_track_id
  }
  ```

### `encodingSwitched`

* Informs that track with id `trackId` belonging to endpoint with id `endpointId` will be sent in encoding `encoding` now.
The meaning is as follows:
* "low_bandwidth" - we no longer have enough bandwidth to maintain current track quality
* "encoding_inactive" - encoding became inactive
* "other" - it was hard to determine exact reason of encoding switch

  ```json
  {
    endpointId: endpoint_id,
    trackId: track_id,
    encoding: encoding,
    reason: "low_bandwidth" | "encoding_inactive" | "other"
  }
  ```

### `vadNotification`
* Informs that the track denoted by `trackId` has changed their voice actiivty
* For this notification to work, the server must be configured to use VAD extension
  and the sender must support it.

```json
{
  trackId: track_id,
  status: "silence" | "speech"
}
```

### `bandwidthEstimation`
* Informs about client's available incoming bitrate (a.k.a. download) estimated by the server.
  It is measured in bits per second.
* For this notification to work, the server must be configured to use TWCC extension
  and the sender must support it.

```json
{
  estimation: estimation,
}
```
