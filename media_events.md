#### Generic media events

<table>
<tr> RTC Engine receives these types of notifications: </tr>
<tr>
    <td>Type</td>
    <td>Format</td>
    <td>Description</td>
</tr>
<tr>
    <td> custom </td>
    <td> <pre lang="elixir"> {:custom_media_event, data} </pre> </td>
    <td> <pre> custom message forwarded to specific peer </pre> </td>
</tr>
<tr>
    <td> track ready </td>
    <td> <pre lang="elixir"> {:track_ready, track_id, encoding, depayloading_filter} </pre> </td>
    <td> notification sent when first packet of track came to endpoint </td>
</tr>
<tr>
    <td> subscribe </td>
    <td> <pre lang="elixir"> {:subscribe, tracks} </pre> </td>
    <td> notification contains list of tracks on which endpoint subscribes itself </td>
</tr>
<tr>
    <td> publish </td>
    <td> <pre lang="elixir"> {:publish, msg} </pre> </td>
    <td> notification contains message, which will be forwarded to all endpoints </td>
</tr>
<tr></tr>
<tr>
    <td> publish new tracks </td>
    <td> <pre lang="elixir"> {:publish, {:new_tracks, tracks}} </pre> </td>
    <td> message informs that new tracks arrives to engine </td>
</tr>
<tr></tr>
<tr>
    <td> publish removed tracks  </td>
    <td> <pre lang="elixir "> {:publish, {:removed_tracks, tracks}} </pre> </td>
    <td> message informs that some tracks where removed by one of endpoint from engine </td>
</tr>

<tr>RTC Engine receives these types of media_events from client:</tr>

<tr>
    <td> join </td>
    <td> <pre lang="json"> 
    {
        "metadata": any
    }</pre> </td>
    <td> message is sent when peer join RTC Engine </td>    
</tr>
<tr>
    <td> leave </td>
    <td> <pre lang="json"> {} </pre> </td>
    <td>  message is sent when peer leave RTC Engine </td>
</tr>
<tr>
    <td> updatePeerMetadata </td>
    <td> <pre lang="json"> 
    {
        "metadata": any
    } </pre> </td>
    <td>  message contains new metadata of peer </td>
</tr>
<tr>
    <td> updateTrackMetadata </td>
    <td> <pre lang="json"> 
    {
        "trackId": track_id,
        "trackMetadata": any
    } </pre> </td>
    <td>  message contains new metadata of track </td>
</tr>

<tr>
    <td> custom </td>
    <td> <pre lang="json"> any </pre> </td>
    <td>  message is forwarded to the endpoint associated with the peer </td>
</tr>


<tr> RTC Engine sends these types of messages to the client: </tr>

<tr>
    <td> tracksPriority </td>
    <td> <pre lang="json"> 
    {
        tracks: tracks
    } </pre> </td>
    <td>  message contains all tracks that will be forwarded to peer untill next tracks_priority message </td>
</tr>
<tr>
    <td> peerDenied </td>
    <td> <pre lang="json"> 
    {
        data: text
    }</pre> </td>
    <td> message sent, if peer was rejected by server during joining to server </td>
</tr>
<tr>
    <td> peerUpdated </td>
    <td> <pre lang="json"> 
    {
        peerId: peer_id,
        metadata: any
    } </pre> </td>
    <td>  message contains new metadata of peer </td>
</tr>
<tr>
    <td> trackUpdated </td>
    <td> <pre lang="json"> 
    {
        peerId: peer_id,
        trackId: track_id,
        metadata: any
    } </pre> </td>
    <td>  message contains new metadata of track </td>
</tr>
<tr>
    <td> custom </td>
    <td> <pre lang="json"> any </pre> </td>
    <td> custom message forwarded from endpoint to client </td>
</tr>
<tr>
    <td> tracksAdded </td>
    <td> <pre lang="json"> 
    {
        peerId: peer_id,
        trackIdToMetadata: { track_id: any }
    } </pre> </td>
    <td> message sent after some tracks added by some peer </td>
</tr>
<tr>
    <td> tracksRemoved </td>
    <td> <pre lang="json"> 
    {
        peerId: peer_id,
        trackIds: track_ids
    } </pre> </td>
    <td>  message contains list of tracks which are removed by some peer </td>
</tr>
<tr>
    <td> peerJoined </td>
    <td> <pre lang="json"> 
    {
        peer: %{
          id: peer.id,
          metadata: peer.metadata
        }
    } </pre> </td>
    <td> message is sent after new peer joined RTC Engine </td>
</tr>
<tr>
    <td> peerAccepted </td>
    <td> <pre lang="json"> 
    {
        id: peer_id,
        peersInRoom: peers
    } </pre> </td>
    <td> message is sent to peer after he join RTC Engine </td>
</tr>
<tr>
    <td> peerLeft </td>
    <td> <pre lang="json"> 
    {
        peerId: peer_id
    } </pre> </td>
    <td> message is sent, when some peer left </td>
</tr>

<tr> WebRTC custom media events </tr>

<tr>WebRTC endpoint receives these types of custom_media_events from client </tr>

<tr>
    <td> custom </td>
    <td> <pre lang="json"> {} </pre> </td>
    <td>  message is forwarded to the endpoint associated with the peer </td>
</tr>

</table>


#### WebRTC custom media events

WebRTC endpoint receives these types of custom_media_events from client:
* renegotiate_tracks - this message informs that a peer wants to make renegotiation due to adding a track or removing a track
* prioritize_track - this message contains id of track, that peer want to prioritize.
* unprioritize_track - this message contains id of track, that peer want to unprioritize.
* prefered_video_sizes - this message contains information of how many videos in different quality (high, medium, small).
There is also flag which indicates that all videos should be in the same quality.
* candidate - this message contains information about ICE candidate it is forwarded to EndpointBin
* sdp_offer - this message contains SDP offer, map track_id to track_metadata and map mid to track_id.
Both maps contains information only about tracks send by peer associated with this endpoint.

WebRTC endpoint sends these type of custom messages to client (by sending it first to RTC Engine):
* offer_data - this message contains information about the number of tracks of audio and video that will be sent from engine
to peer and information regarding the integrated TURN server.
* candidate - this message contains information about ICE candidate which will be sent to the client.
* sdp_answer - this message contains SDP answer and map mid to track_id.


<table>
<tr>
    <td> </td>
    <td> </td>
    <td> <pre lang="json"> </pre> </td>
</tr>
<tr>
    <td> </td>
    <td> </td>
    <td> <pre lang="json"> </pre> </td>
</tr>

</table>
