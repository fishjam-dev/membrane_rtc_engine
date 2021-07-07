import { SerializedMediaEvent } from "./mediaEvent";
/**
 * Interface describing Peer.
 */
export interface Peer {
    /**
     * Peer's id. It is assigned by user in custom logic that use backend API.
     */
    id: string;
    /**
     * Any information that was provided in {@link join}.
     */
    metadata: any;
    /**
     * mapping between track's mid and its metadata. Track metadata
     * can be set using {@link addTrack}
     */
    midToTrackMetadata: any;
}
/**
 * Config passed to {@link MembraneWebRTC}.
 */
export interface MembraneWebRTCConfig {
    callbacks: Callbacks;
    rtcConfig?: RTCConfiguration;
    /**
     * Determines wheater user want to receive media from other peers.
     */
    receiveMedia?: boolean;
    peerConfig: PeerConfig;
}
/**
 * Track's context i.e. all data that can be usful when operating on track.
 */
export interface TrackContext {
    track: MediaStreamTrack;
    /**
     * Stream this track belongs to.
     */
    stream: MediaStream;
    /**
     * Peer this track comes from.
     */
    peer: Peer;
    mid: string;
    /**
     * Any info that was passed in {@link addTrack}.
     */
    metadata: any;
}
/**
 * Peer's configuration. It is used by a server to create proper SDP offer.
 * At this moment there is no possibility to send more than two tracks.
 * This interface should probably be removed after adding ability to send
 * any number of tracks.
 */
export interface PeerConfig {
    /**
     * Determines if user is willing to send video track.
     */
    relayVideo: boolean;
    /**
     * Determines if user is willing to send audio track.
     */
    relayAudio: boolean;
}
/**
 * Callbacks that has to be implemented by user.
 */
export interface Callbacks {
    /**
     * Called each time MembraneWebRTC need to send some data to the server.
     */
    onSendMediaEvent: (mediaEvent: SerializedMediaEvent) => void;
    /**
     * Called when peer is accepted. Triggered by {@link join}
     */
    onJoined?: (peerId: string, peersInRoom: [Peer]) => void;
    /**
     * Called when peer was not accepted. Triggered by {@link join}
     */
    onDenied?: () => void;
    /**
     * Called when a new track appears.
     *
     * This callback is always called after a new peer joins so after calling {@link onPeerJoined}.
     * @param ctx - Contains information about the new track.
     */
    onTrackAdded?: (ctx: TrackContext) => void;
    /**
     * Called when some track will no longer be sent.
     *
     * At this moment there is only one situation in which this callback is invoked i.e. when peer
     * leaves the room. In such scenario, this callback will be invoked for each track this peer
     * was sending and then {@link onPeerLeft} will be called.
     */
    onTrackRemoved?: (ctx: TrackContext) => void;
    /**
     * Called each time new peer joins the room.
     */
    onPeerJoined?: (peer: Peer) => void;
    /**
     * Called each time peer leaves the room.
     */
    onPeerLeft?: (peer: Peer) => void;
    /**
     * Called in case of errors related to multimedia session e.g. ICE connection.
     */
    onConnectionError?: (message: string) => void;
}
/**
 * Main class that is responsible for connecting to the SFU server, sending and receiving media.
 */
export declare class MembraneWebRTC {
    private id?;
    private receiveMedia;
    private localTracksWithStreams;
    private midToTrackMetadata;
    private localTrackIdToMetadata;
    private midToStream;
    private connection?;
    private idToPeer;
    private midToPeer;
    private readonly rtcConfig;
    private readonly callbacks;
    constructor(config: MembraneWebRTCConfig);
    /**
     * Tries to join to the SFU server. If user is accepted then {@link onJoined}
     * will be called. In other case {@link onDenied} is invoked.
     *
     * @param peerMetadata - Any information that other peers will receive in {@link onPeerJoined}
     * after accepting this peer
     *
     * @example
     * ```ts
     * let webrtc = new MembraneWebRTC(...)
     * webrtc.join({displayName: "Bob"})
     * ```
     */
    join: (peerMetadata: any) => void;
    /**
     * Feeds media event received from SFU server to {@link MembraneWebRTC}.
     * This function should be called whenever some media event from SFU server
     * was received and can result in {@link MembraneWebRTC} generating some other
     * media events.
     *
     * @param mediaEvent - String data received over custom signalling layer.
     *
     * @example
     * This example assumes pheonix channels as signalling layer.
     * As pheonix channels require objects, SFU server encapsulates binary data into
     * map with one field that is converted to object with one field on the TS side.
     * ```ts
     * webrtcChannel.on("mediaEvent", (event) => webrtc.receiveMediaEvent(event.data));
     * ```
     */
    receiveMediaEvent: (mediaEvent: SerializedMediaEvent) => void;
    /**
     * Adds track that will be sent to the SFU server.
     * @param track - Audio or video track e.g. from your microphone or camera.
     * @param stream  - Stream that this track belongs to.
     * @param trackMetadata - Any information about this track that other peers will
     * receive in {@link onPeerJoined}. E.g. this can source of the track - wheather it's
     * screensharing, webcam or some other media device.
     *
     * @example
     * ```ts
     * let localStream: MediaStream = new MediaStream();
     * try {
     *   localAudioStream = await navigator.mediaDevices.getUserMedia(
     *     AUDIO_CONSTRAINTS
     *   );
     *   localAudioStream
     *     .getTracks()
     *     .forEach((track) => localStream.addTrack(track));
     * } catch (error) {
     *   console.error("Couldn't get microphone permission:", error);
     * }
     *
     * try {
     *   localVideoStream = await navigator.mediaDevices.getUserMedia(
     *     VIDEO_CONSTRAINTS
     *   );
     *   localVideoStream
     *     .getTracks()
     *     .forEach((track) => localStream.addTrack(track));
     * } catch (error) {
     *  console.error("Couldn't get camera permission:", error);
     * }
     *
     * localStream
     *  .getTracks()
     *  .forEach((track) => webrtc.addTrack(track, localStream));
     * ```
     */
    addTrack(track: MediaStreamTrack, stream: MediaStream, trackMetadata?: any): void;
    /**
     * Leaves the room. This function should be called when user leaves the room
     * in a clean way e.g. by clicking dedicated, custom button `dissconnetc`.
     * As a result there will be generated one more media event that should be
     * sent to the SFU server. Thanks to it each other peer will be notified
     * that peer left in {@link onPeerLeft},
     */
    leave: () => void;
    /**
     * Cleans up {@link MembraneWebRTC} instance.
     */
    cleanUp: () => void;
    private onOffer;
    private onRemoteCandidate;
    private onLocalCandidate;
    private onTrack;
    private onPeerJoined;
    private onPeerLeft;
    private addPeer;
    private removePeer;
}
//# sourceMappingURL=membraneWebRTC.d.ts.map