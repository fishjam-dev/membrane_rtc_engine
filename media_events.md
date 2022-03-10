### Generic media events

#### RTC Engine receives these types of media_events from client:
| Name | Description |
| ---- | ----------- |
| [join](#join) | message is sent when peer join RTC Engine |
| [leave](#leave) | message is sent when peer leave RTC Engine |
| [updatePeerMetadata](#updatepeermetadata) | message contains new metadata of peer |
| [updateTrackMetadata](#updatetrackmetadata) | message contains new metadata of track |
| [custom](#custom) | message is forwarded to the endpoint associated with the peer |



#### RTC Engine sends these types of messages to the client: 

| Name | Description |
| ---- | ----------- |
| [tracksPriority](#trackspriority) | message contains all tracks that will be forwarded to peer untill next tracks_priority message |
| [peerDenied](#peerdenied) | message sent, if peer was rejected by server during joining to server |
| [peerUpdated](#peerupdated) | message contains new metadata of peer |
| [trackUpdated](#trackupdated) | message contains new metadata of track |
| [custom](#custom-1) | custom message forwarded from endpoint to client |
| [tracksAdded](#tracksadded) | message sent after some tracks added by some peer |
| [tracksRemoved](#tracksremoved) | message contains list of tracks which are removed by some peer |
| [peerJoined](#peerjoined) | message is sent after new peer joined RTC Engine |
| [peerAccepted](#peeraccepted) | message is sent to peer after he join RTC Engine |
| [peerLeft](#peerleft) | message is sent, when some peer left |

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
* message is sent when peer join RTC Engine 
* ```json 
    {
        "metadata": any
    }
    ```
### leave 
* message is sent when peer leave RTC Engine
* ```json 
    {}
    ```
### updatePeerMetadata
* message contains new metadata of peer
* ```json
    {
    "metadata": any
    }
    ``` 
### updateTrackMetadata
* message contains new metadata of track
* ```json
    {
        "trackId": track_id,
        "trackMetadata": any
    }
    ```
### custom 
* message is forwarded to the endpoint associated with the peer
* ```json
    any 
    ```

### tracksPriority
* message contains all tracks that will be forwarded to peer untill next tracks_priority message
* ```json
    {
        tracks: tracks
    }
    ``` 
### peerDenied
* message sent, if peer was rejected by server during joining to server
* ```json 
    {
        data: text
    }
    ```
### peerUpdated
* message contains new metadata of peer
* ```json 
    {
        peerId: peer_id,
        metadata: any
    }
    ```
### trackUpdated
* message contains new metadata of track
* ```json 
    {
        peerId: peer_id,
        trackId: track_id,
        metadata: any
    }
    ```
### custom 
* custom message forwarded from endpoint to client
* ```json
    any 
    ```
### tracksAdded
* message sent after some tracks added by some peer
* ```json
    {
        peerId: peer_id,
        trackIdToMetadata: { 
            track_id: any 
        }
    }
    ```
### tracksRemoved
* message contains list of tracks which are removed by some peer
* ```json
    {
        peerId: peer_id,
        trackIds: track_ids
    }
    ```
### peerJoined
* message is sent after new peer joined RTC Engine
* ```json
    {
        peer: {
            id: peer.id,
            metadata: peer.metadata
        }
    }
    ```
### peerAccepted
* message is sent to peer after he join RTC Engine
* ```json
    {
        id: peer_id,
        peersInRoom: peers
    }
    ```
### peerLeft 
* message is sent, when some peer left
* ```json 
    {
        peerId: peer_id
    }
    ```
### renegotiateTracks 
* message informs that a peer wants to make renegotiation 
* ```json
    {} 
    ``` 
### prioritizeTrack 
* message informs about, which track peer want to prioritize 
* ```json
    {
        trackId: trackId
    } 
    ``` 
### unprioritizeTrack 
* message informs about, which track peer want to unprioritize 
* ```json
    {
        trackId: trackId
    } 
    ``` 
### prefereVideoSizes 
* message informs of how many videos in different quality, peer wants to receive 
* ```json
    {
        bigScreens: Int, 
        mediumScreens: Int, 
        smallScreens: Int, 
        allSameSize: boolean
    } 
    ``` 
### candidate 
* message informs about ICE candidate
* ```json
    {
        candidate: candidate,
        sdpMLineIndex: Int
    }
    ``` 
### sdpOffer 
* message informs all needed information for SDP negotiation
* ```json
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
* message informs about data needed 
* ```json
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
* message informs about ICE candidate 
* ```json
    {
        candidate: candidate,
        sdpMLineIndex: sdp_m_line_index,
        sdpMid: nil,
        usernameFragment: nil
    }
    ``` 
### sdpAnswer 
* message informs about SDP Answer 
* ```json
    {
        type: "answer",
        sdp: sdp_answer,
        midToTrackId: mid_to_track_id
    } 
    ``` 