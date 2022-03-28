# Media Events

Below are described all messages, which are sent between Engine and client. 
Firstly there are tables with names and short descriptions. On the end, there are bullet points with a longer description and format of each media_event. Each media event have to be sent in the form:
```json
{
    type: name,
    data: data
}
```
The format described on the end represents data from the above JSON, where the name of the message is the value of the type field.

#### RTC Engine receives these types of media_events from client:
| Name | Description |
| ---- | ----------- |
| [join](#join) | message is sent when peer join RTC Engine |
| [leave](#leave) | message is sent when peer leave RTC Engine |
| [updatePeerMetadata](#updatepeermetadata) | message contains new metadata of peer |
| [updateTrackMetadata](#updatetrackmetadata) | message contains new metadata of track |
| [custom](#custom) | message is forwarded to the endpoint associated with the peer |
| [selectEncoding] (#selectencoding) | message informs that a peer wants to receive specific encoding of some track |


#### RTC Engine sends these types of messages to the client: 

| Name | Description |
| ---- | ----------- |
| [tracksPriority](#trackspriority) | message contains all tracks that will be forwarded to peer until next tracks_priority message |
| [peerDenied](#peerdenied) | message sent, if peer was rejected by server during joining to server |
| [peerUpdated](#peerupdated) | message contains new metadata of peer |
| [trackUpdated](#trackupdated) | message contains new metadata of track |
| [custom](#custom-1) | custom message forwarded from endpoint to client |
| [tracksAdded](#tracksadded) | message sent after some tracks added by some peer |
| [tracksRemoved](#tracksremoved) | message contains list of tracks which are removed by some peer |
| [peerJoined](#peerjoined) | message is sent after new peer joined RTC Engine |
| [peerAccepted](#peeraccepted) | message is sent to peer after he join RTC Engine |
| [peerLeft](#peerleft) | message is sent, when some peer left |
| [encodingSwitched](#encodingswitched) | message is sent when track is now sent in a new encoding |
| [peerRemoved](#peerRemoved) | message is sent when peer is unwillingly removed by the server |

### WebRTC custom media events

#### WebRTC endpoint receives these types of custom_media_events from client:

| Name | Description |
| ---- | ----------- |
| [renegotiateTracks](#renegotiatetracks) | message informs that a peer wants to make renegotiation |
| [prioritizeTrack](#prioritizetrack) | message informs about, which track peer want to prioritize |
| [unprioritizeTrack](#unprioritizetrack) | message informs about, which track peer want to unprioritize |
| [prefereVideoSizes](#preferevideosizes) | message informs of how many videos in different quality, peer wants to receive |
| [candidate](#candidate) | message informs about ICE candidate|
| [sdpOffer](#sdpoffer) | message informs all needed information for SDP negotiation|

#### WebRTC endpoint sends these type of custom messages to client
| Name | Description |
| ---- | ----------- |
| [offerData](#offerdata) | message informs about data needed |
| [candidate](#candidate-1) | message informs about ICE candidate |
| [sdpAnswer](#sdpanswer) | message informs about SDP Answer |


### join 
* this message is sent when peer want to join RTC Engine. It contains only peer metadata
* 
```json 
    {
        "metadata": any
    }
```
### leave 
* this message is sent when peer leave RTC Engine. It contains no data
* 
```json 
    {}
```
### updatePeerMetadata
* this message contains new metadata of some peer
* 
```json
    {
    "metadata": any
    }
``` 
### updateTrackMetadata
* this message contains new metadata of some track and id of this track
* 
```json
    {
        "trackId": track_id,
        "trackMetadata": any
    }
```
### custom 
* this message is a black box for RTC Engine and it is forwarded to the endpoint associated with the peer
* 
```json
    any 
```

### tracksPriority
* this message contains all tracks that will be forwarded to peer untill next tracks_priority message this type of messages is sent only if DisplayManager is spawned
* 
```json
    {
        tracks: tracks
    }
``` 
### peerDenied
* this message is sent, if peer was rejected by server during joining to server. It can cointains a information why he was rejected
* 
```json 
    {
        data: text
    }
```
### peerUpdated
* this message contains information about updated metadata of one of peers
* 
```json 
    {
        peerId: peer_id,
        metadata: any
    }
```
### trackUpdated
* this message contains information about new metadata of one of tracks
* 
```json 
    {
        peerId: peer_id,
        trackId: track_id,
        metadata: any
    }
```
### custom 
* this is custom message forwarded by engine from endpoint to client
* 
```json
    any 
```
### tracksAdded
* this message informs that one of the peers add track. It contains map track_id to track_metadata of all tracks of one of the peers and id of that peer
* 
```json
    {
        peerId: peer_id,
        trackIdToMetadata: { 
            track_id: any 
        }
    }
```
### tracksRemoved
* this message contains list of tracks which are removed by some peer and id of that peer
* 
```json
    {
        peerId: peer_id,
        trackIds: track_ids
    }
```
### peerJoined
* this message is sent to all peers in room after new peer joined RTC Engine. It contains id and metadata of new peer.
* 
```json
    {
        peer: {
            id: peer.id,
            metadata: peer.metadata
        }
    }
```
### peerAccepted
* this message is sent to peer after he join RTC Engine.
  It contains his id and list of information about each peer in Engine (id, metadata and map track_id to track_metadata)
* 
```json
    {
        id: peer_id,
        peersInRoom: peers
    }
```
### peerLeft 
* this message is sent to all other peers in room, when some peer left. It contains id of peer which left.
* 
```json 
    {
        peerId: peer_id
    }
```
### renegotiateTracks 
* this message informs that a peer wants to make renegotiation due to adding a track or removing a track 
* 
```json
    {} 
``` 
### prioritizeTrack 
* this message contains id of track, that peer want to prioritize
* 
```json
    {
        trackId: trackId
    } 
``` 
### unprioritizeTrack 
* this message contains id of track, that peer want to unprioritize 
* 
```json
    {
        trackId: trackId
    } 
``` 
### prefereVideoSizes 
* this message contains information of how many videos in different quality (high, medium, small). There is also flag which indicates that all videos should be in the same quality.
* 
```json
    {
        bigScreens: Int, 
        mediumScreens: Int, 
        smallScreens: Int, 
        allSameSize: boolean
    } 
``` 
### candidate 
* this message contains information about ICE candidate it is forwarded to EndpointBin
* 
```json
    {
        candidate: candidate,
        sdpMLineIndex: Int
    }
``` 
### sdpOffer 
* this message contains SDP offer, map track_id to track_metadata and map mid to track_id. Both maps contains information only about tracks send by peer associated with this endpoint.
* 
```json
    {
        sdpOffer: {
            type: "offer",
            sdp: sdp_offer
        },
        trackIdToTrackMetadata: {
            trackId: any
        },
        midToTrackId: mid_to_track_id
    }
``` 
### offerData 
* this message contains information about the number of tracks of audio and video that will be sent from engine
to peer and information regarding the integrated TURN server. 
* 
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
        }],
        iceTransportPolicy: "relay" | "all"
    }
``` 
### candidate 
* this message contains information about ICE candidate which will be sent to the client 
* 
```json
    {
        candidate: candidate,
        sdpMLineIndex: sdp_m_line_index,
        sdpMid: nil,
        usernameFragment: nil
    }
``` 
### sdpAnswer 
* this message contains SDP answer and map mid to track_id 
* 
```json
    {
        type: "answer",
        sdp: sdp_answer,
        midToTrackId: mid_to_track_id
    } 
``` 
### selectEncoding
* this message informs that a peer wants to receive specific encoding of some track
* 
```json
    {
        peerId => peer_id,
        trackId => track_id,
        encoding => encoding
    }
```
### encodingSwitched
* this message informs that track with id `trackId` belonging to peer with id `peerId` will be sent in encoding `encoding` now.
* 
```json
    {
        peerId: peer_id,
        trackId: track_id,
        encoding: encoding    
    } 
```    
