### Generic media events

##### RTC Engine receives these types of media_events from client:
<table>
<tr>
    <th> Name </th>
    <th> Description </th>
</tr>
<tr>
    <td> join </td>
    <td> message is sent when peer join RTC Engine </td>    
</tr>
<tr>
    <td> leave </td>
    <td> message is sent when peer leave RTC Engine </td>
</tr>
<tr>
    <td> updatePeerMetadata </td>
    <td> message contains new metadata of peer </td>
</tr>
<tr>
    <td> updateTrackMetadata </td>
    <td> message contains new metadata of track </td>
</tr>

<tr>
    <td> custom </td>
    <td> message is forwarded to the endpoint associated with the peer </td>
</tr>

</table>

##### RTC Engine sends these types of messages to the client: 

<table>
<tr>
    <th> Name </th>
    <th> Description </th>
</tr>
<tr>
    <td> tracksPriority </td>
    <td> message contains all tracks that will be forwarded to peer untill next tracks_priority message </td>
</tr>
<tr>
    <td> peerDenied </td>
    <td> message sent, if peer was rejected by server during joining to server </td>
</tr>
<tr>
    <td> peerUpdated </td>
    <td> message contains new metadata of peer </td>
</tr>
<tr>
    <td> trackUpdated </td>
    <td> message contains new metadata of track </td>
</tr>
<tr>
    <td> custom </td>
    <td> custom message forwarded from endpoint to client </td>
</tr>
<tr>
    <td> tracksAdded </td>
    <td> message sent after some tracks added by some peer </td>
</tr>
<tr>
    <td> tracksRemoved </td>
    <td> message contains list of tracks which are removed by some peer </td>
</tr>
<tr>
    <td> peerJoined </td>
    <td> message is sent after new peer joined RTC Engine </td>
</tr>
<tr>
    <td> peerAccepted </td>
    <td> message is sent to peer after he join RTC Engine </td>
</tr>
<tr>
    <td> peerLeft </td>
    <td> message is sent, when some peer left </td>
</tr>
</table>

### WebRTC custom media events

##### WebRTC endpoint receives these types of custom_media_events from client:

<table>
<tr>
    <th> Name </th>
    <th> Description </th>
</tr>
<tr>
    <td> renegotiateTracks </td>
    <td> message informs that a peer wants to make renegotiation </td>
</tr>
<tr>
    <td> prioritizeTrack </td>
    <td> message informs about, which track peer want to prioritize </td>
</tr>
<tr>
    <td> unprioritizeTrack </td>
    <td> message informs about, which track peer want to unprioritize </td>
</tr>
<tr>
    <td> prefereVideoSizes </td>
    <td> message informs of how many videos in different quality, peer wants to receive </td>
</tr>
<tr>
    <td> candidate </td>
    <td> message informs about ICE candidate</td>
</tr>
<tr>
    <td> sdpOffer </td>
    <td> message informs all needed information for SDP negotiation</td>
</tr>
</table>

##### WebRTC endpoint sends these type of custom messages to client
<table>
<tr>
    <th> Name </th>
    <th> Description </th>
</tr>
<tr>
    <td> offerData </td>
    <td> message informs about data needed </td>
</tr>
<tr>
    <td> candidate </td>
    <td> message informs about ICE candidate </td>
</tr>
<tr>
    <td> sdpAnswer </td>
    <td> message informs about SDP Answer </td>
</tr>
</table>

##### RTC Engine receives these types of media_events from client:
* join 
    * message is sent when peer join RTC Engine 
    * ```json 
        {
            "metadata": any
        }
        ```
* leave 
    * message is sent when peer leave RTC Engine
    * ```json 
        {}
        ```
* updatePeerMetadata
    * message contains new metadata of peer
    * ```json
        {
        "metadata": any
        }
        ``` 
* updateTrackMetadata
    * message contains new metadata of track
    * ```json
        {
            "trackId": track_id,
            "trackMetadata": any
        }
        ```
* custom 
    * message is forwarded to the endpoint associated with the peer
    ```json
    any 
    ```

##### RTC Engine sends these types of messages to the client: 

* tracksPriority
    * message contains all tracks that will be forwarded to peer untill next tracks_priority message
    * ```json
        {
            tracks: tracks
        }
        ``` 
* peerDenied
    * message sent, if peer was rejected by server during joining to server
    * ```json 
        {
            data: text
        }
        ```
* peerUpdated
    * message contains new metadata of peer
    * ```json 
        {
            peerId: peer_id,
            metadata: any
        }
        ```
* trackUpdated
    * message contains new metadata of track
    * ```json 
        {
            peerId: peer_id,
            trackId: track_id,
            metadata: any
        }
        ```
* custom 
    * custom message forwarded from endpoint to client
    * ```json
        any 
        ```
* tracksAdded
    * message sent after some tracks added by some peer
    * ```json
        {
            peerId: peer_id,
            trackIdToMetadata: { 
                track_id: any 
            }
        }
        ```
* tracksRemoved
    * message contains list of tracks which are removed by some peer
    * ```json
        {
            peerId: peer_id,
            trackIds: track_ids
        }
        ```
* peerJoined
    * message is sent after new peer joined RTC Engine
    * ```json
        {
            peer: {
                id: peer.id,
                metadata: peer.metadata
            }
        }
        ```
* peerAccepted
    * message is sent to peer after he join RTC Engine
    * ```json
        {
            id: peer_id,
            peersInRoom: peers
        }
        ```
* peerLeft 
    * message is sent, when some peer left
    ```json 
        {
            peerId: peer_id
        }
    ```

### WebRTC custom media events

##### WebRTC endpoint receives these types of custom_media_events from client:

* renegotiateTracks 
    * message informs that a peer wants to make renegotiation 
    * ```json
        {} 
        ``` 
* prioritizeTrack 
    * message informs about, which track peer want to prioritize 
    * ```json
        {
            trackId: trackId
        } 
        ``` 
* unprioritizeTrack 
    * message informs about, which track peer want to unprioritize 
    * ```json
        {
            trackId: trackId
        } 
        ``` 
* prefereVideoSizes 
    * message informs of how many videos in different quality, peer wants to receive 
    * ```json
        {
            bigScreens: Int, 
            mediumScreens: Int, 
            smallScreens: Int, 
            allSameSize: boolean
        } 
        ``` 
* candidate 
    * message informs about ICE candidate
    * ```json
        {
            candidate: candidate,
            sdpMLineIndex: Int
        }
        ``` 
* sdpOffer 
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
##### WebRTC endpoint sends these type of custom messages to client

* offerData 
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
* candidate 
    * message informs about ICE candidate 
    * ```json
        {
            candidate: candidate,
            sdpMLineIndex: sdp_m_line_index,
            sdpMid: nil,
            usernameFragment: nil
        }
        ``` 
* sdpAnswer 
    * message informs about SDP Answer 
    * ```json
        {
            type: "answer",
            sdp: sdp_answer,
            midToTrackId: mid_to_track_id
        } 
        ``` 