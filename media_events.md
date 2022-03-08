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
    <td> <pre> custom message forwarded to specific peer </pre> </td>
    <td> <pre lang="elixir"> {:custom_media_event, data} </pre> </td>
</tr>
<tr>
    <td> track ready </td>
    <td> notification sent when first packet of track came to endpoint </td>
    <td> <pre lang="elixir"> {:track_ready, track_id, encoding, depayloading_filter} </pre> </td>
</tr>
<tr>
    <td> subscribe </td>
    <td> notification contains list of tracks on which endpoint subscribes itself </td>
    <td> <pre lang="elixir"> {:subscribe, tracks} </pre> </td>
</tr>
<tr>
    <td> publish </td>
    <td> notification contains message, which will be forwarded to all endpoints </td>
    <td> <pre lang="elixir"> {:publish, msg} </pre> </td>
</tr>
<tr></tr>
<tr>
    <td> publish new tracks </td>
    <td> message informs that new tracks arrives to engine </td>
    <td> <pre lang="elixir"> {:publish, {:new_tracks, tracks}} </pre> </td>
</tr>
<tr></tr>
<tr>
    <td> publish removed tracks  </td>
    <td> message informs that some tracks where removed by one of endpoint from engine </td>
    <td> <pre lang="elixir "> {:publish, {:removed_tracks, tracks}} </pre> </td>
</tr>

<tr>RTC Engine receives these types of media_events from client:</tr>

<tr>
    <td> join </td>
    <td> message is sent when peer join RTC Engine </td>    
    <td> <pre lang="json"> 
    {
        "metadata": any
    }</pre> </td>
</tr>
<tr>
    <td> leave </td>
    <td>  message is sent when peer leave RTC Engine </td>
    <td> <pre lang="json"> {} </pre> </td>
</tr>
<tr>
    <td> updatePeerMetadata </td>
    <td>  message contains new metadata of peer </td>
    <td> <pre lang="json"> 
    {
        "metadata": any
    } </pre> </td>
</tr>
<tr>
    <td> updateTrackMetadata </td>
    <td>  message contains new metadata of track </td>
    <td> <pre lang="json"> 
    {
        "trackId": track_id,
        "trackMetadata": any
    } </pre> </td>
</tr>

<tr>
    <td> custom </td>
    <td>  message is forwarded to the endpoint associated with the peer </td>
    <td> <pre lang="json"> any </pre> </td>
</tr>


<tr> RTC Engine sends these types of messages to the client: </tr>

<tr>
    <td> tracksPriority </td>
    <td>  message contains all tracks that will be forwarded to peer untill next tracks_priority message </td>
    <td> <pre lang="json"> 
    {
        tracks: tracks
    } </pre> </td>
</tr>
<tr>
    <td> peerDenied </td>
    <td> message sent, if peer was rejected by server during joining to server </td>
    <td> <pre lang="json"> 
    {
        data: text
    }</pre> </td>
</tr>
<tr>
    <td> peerUpdated </td>
    <td>  message contains new metadata of peer </td>
    <td> <pre lang="json"> 
    {
        peerId: peer_id,
        metadata: any
    } </pre> </td>
</tr>
<tr>
    <td> trackUpdated </td>
    <td>  message contains new metadata of track </td>
    <td> <pre lang="json"> 
    {
        peerId: peer_id,
        trackId: track_id,
        metadata: any
    } </pre> </td>
</tr>
<tr>
    <td> custom </td>
    <td> custom message forwarded from endpoint to client </td>
    <td> <pre lang="json"> any </pre> </td>
</tr>
<tr>
    <td> tracksAdded </td>
    <td> message sent after some tracks added by some peer </td>
    <td> <pre lang="json"> 
    {
        peerId: peer_id,
        trackIdToMetadata: { track_id: any }
    } </pre> </td>
</tr>
<tr>
    <td> tracksRemoved </td>
    <td>  message contains list of tracks which are removed by some peer </td>
    <td> <pre lang="json"> 
    {
        peerId: peer_id,
        trackIds: track_ids
    } </pre> </td>
</tr>
<tr>
    <td> peerJoined </td>
    <td> message is sent after new peer joined RTC Engine </td>
    <td> <pre lang="json"> 
    {
        peer: %{
          id: peer.id,
          metadata: peer.metadata
        }
    } </pre> </td>
</tr>
<tr>
    <td> peerAccepted </td>
    <td> message is sent to peer after he join RTC Engine </td>
    <td> <pre lang="json"> 
    {
        id: peer_id,
        peersInRoom: peers
    } </pre> </td>
</tr>
<tr>
    <td> peerLeft </td>
    <td> message is sent, when some peer left </td>
    <td> <pre lang="json"> 
    {
        peerId: peer_id
    } </pre> </td>
</tr>

<tr> WebRTC custom media events </tr>

<tr>WebRTC endpoint receives these types of custom_media_events from client </tr>

<tr>
    <td> renegotiate_tracks </td>
    <td> message informs that a peer wants to make renegotiation </td>
    <td> <pre lang="json"> {} </pre> </td>
</tr>
<tr>
    <td> prioritize_track </td>
    <td> message informs about, which track peer want to prioritize </td>
    <td> <pre lang="json"> 
    {
        trackId: trackId
    } </pre> </td>
</tr>
<tr>
    <td> unprioritize_track </td>
    <td> message informs about, which track peer want to unprioritize </td>
    <td> <pre lang="json"> 
    {
        trackId: trackId
    } </pre> </td>
</tr>
<tr>
    <td> prefered_video_sizes </td>
    <td> message informs of how many videos in different quality, peer wants to receive </td>
    <td> <pre lang="json"> 
    {
        bigScreens: Int, 
        mediumScreens: Int, 
        smallScreens: Int, 
        allSameSize: boolean
    } </pre> </td>
</tr>
<tr>
    <td> candidate </td>
    <td> message informs about ICE candidate</td>
    <td> <pre lang="json"> 
    {
        candidate: candidate,
        sdpMLineIndex: sdp_m_line_index
    }</pre> </td>
</tr>
<tr>
    <td> sdp_offer </td>
    <td> message informs all needed information for SDP negotiation</td>
    <td> <pre lang="json"> 
    {
        "type" => "sdpOffer",
        "data" => %{
        "sdpOffer" => %{
            "type" => "offer",
            "sdp" => sdp_offer
        },
        "trackIdToTrackMetadata" => track_id_to_track_metadata,
        "midToTrackId" => mid_to_track_id
    }
    }</pre> </td>
</tr>


<tr>WebRTC endpoint sends these type of custom messages to client </tr>
<tr>
    <td> offer_data </td>
    <td> message informs about data needed </td>
    <td> <pre lang="json"> 
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
    }</pre> </td>
</tr>
<tr>
    <td> candidate </td>
    <td> message informs about ICE candidate </td>
    <td> <pre lang="json"> 
    {
        candidate: candidate,
        sdpMLineIndex: sdp_m_line_index,
        sdpMid: nil,
        usernameFragment: nil
    }</pre> </td>
</tr>
<tr>
    <td> sdp_answer </td>
    <td> message informs about SDP Answer </td>
    <td> <pre lang="json"> 
    {
        type: "answer",
        sdp: sdp_answer,
        midToTrackId: mid_to_track_id
    } </pre> </td>
</tr>
</table>
