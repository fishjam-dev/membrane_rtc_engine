export declare type SerializedMediaEvent = string;
export interface Peer {
    id: string;
    metadata: any;
    midToTrackMetadata: any;
}
export interface MembraneWebRTCConfig {
    callbacks: Callbacks;
    rtcConfig?: RTCConfiguration;
    receiveMedia?: boolean;
    peerConfig: PeerConfig;
}
interface TrackContext {
    track: MediaStreamTrack;
    stream: MediaStream;
    peer: Peer;
    mid: string;
    metadata: any;
}
interface PeerConfig {
    relayVideo: boolean;
    relayAudio: boolean;
}
interface Callbacks {
    onSendSerializedMediaEvent: (serializedMediaEvent: SerializedMediaEvent) => void;
    onJoined?: (peerId: string, peersInRoom: [Peer]) => void;
    onDenied?: () => void;
    onTrackAdded?: (ctx: TrackContext) => void;
    onTrackRemoved?: (ctx: TrackContext) => void;
    onPeerJoined?: (peer: Peer) => void;
    onPeerLeft?: (peer: Peer) => void;
    onConnectionError?: (message: string) => void;
}
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
    join: (peerMetadata: any) => void;
    receiveEvent: (serializedMediaEvent: SerializedMediaEvent) => void;
    addLocalTrack(track: MediaStreamTrack, stream: MediaStream, trackMetadata?: any): void;
    leave: () => void;
    cleanUp: () => void;
    private onOffer;
    private onRemoteCandidate;
    private onLocalCandidate;
    private onTrack;
    private onPeerJoined;
    private onPeerLeft;
    private onSendMediaEvent;
    private addPeer;
    private removePeer;
    private generateMediaEvent;
}
export {};
//# sourceMappingURL=membraneWebRTC.d.ts.map