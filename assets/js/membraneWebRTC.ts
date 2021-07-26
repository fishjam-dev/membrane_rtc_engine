import {
  SerializedMediaEvent,
  serializeMediaEvent,
  deserializeMediaEvent,
  generateMediaEvent,
  MediaEvent,
} from "./mediaEvent";
import { DEFAULT_TRANSCEIVER_CONFIG } from "./consts";

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
 * Callbacks that has to be implemented by user.
 */
export interface Callbacks {
  /**
   * Called each time MembraneWebRTC need to send some data to the server.
   */
  onSendMediaEvent: (mediaEvent: SerializedMediaEvent) => void;

  /**
   * Called when peer was accepted. Triggered by {@link join}
   */
  onJoinSuccess?: (peerId: string, peersInRoom: [Peer]) => void;
  /**
   * Called when peer was not accepted. Triggered by {@link join}
   * @param metadata - Passthru for client application to communicate further actions to frontend
   */
  onJoinError?: (metadata: any) => void;

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
export class MembraneWebRTC {
  private id?: string;

  private receiveMedia: boolean;

  private localTracksWithStreams: {
    track: MediaStreamTrack;
    stream: MediaStream;
  }[] = [];
  private midToTrackMetadata: Map<string, any> = new Map();
  private localTrackIdToMetadata: Map<string, any> = new Map();
  private midToStream: Map<String, MediaStream> = new Map();
  private connection?: RTCPeerConnection;
  private idToPeer: Map<String, Peer> = new Map();
  private midToPeer: Map<String, Peer> = new Map();
  private readonly rtcConfig: RTCConfiguration = {
    iceServers: [
      {
        urls: "stun:stun.l.google.com:19302",
      },
    ],
  };

  private readonly callbacks: Callbacks;

  constructor(config: MembraneWebRTCConfig) {
    const { receiveMedia = true, callbacks, rtcConfig } = config;

    this.receiveMedia = receiveMedia;

    this.callbacks = callbacks;
    this.rtcConfig = rtcConfig || this.rtcConfig;
  }

  /**
   * Tries to join to the SFU server. If user is accepted then {@link onJoinSuccess}
   * will be called. In other case {@link onJoinError} is invoked.
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
  public join = (peerMetadata: any): void => {
    try {
      let relayAudio = false;
      let relayVideo = false;

      this.localTracksWithStreams.forEach(({ stream }) => {
        if (stream.getAudioTracks().length != 0) relayAudio = true;
        if (stream.getVideoTracks().length != 0) relayVideo = true;
      });

      let mediaEvent = generateMediaEvent("join", {
        relayAudio: relayAudio,
        relayVideo: relayVideo,
        receiveMedia: this.receiveMedia,
        metadata: peerMetadata,
        tracksMetadata: Array.from(this.localTrackIdToMetadata.values()),
      });
      this.sendMediaEvent(mediaEvent);
    } catch (e) {
      this.callbacks.onConnectionError?.(e);
      this.leave();
    }
  };

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
  public receiveMediaEvent = (mediaEvent: SerializedMediaEvent) => {
    const deserializedMediaEvent = deserializeMediaEvent(mediaEvent);
    let peer;
    switch (deserializedMediaEvent.type) {
      case "peerAccepted":
        this.id = deserializedMediaEvent.data.id;
        this.callbacks.onJoinSuccess?.(
          deserializedMediaEvent.data.id,
          deserializedMediaEvent.data.peersInRoom
        );
        let peers = deserializedMediaEvent.data.peersInRoom as Peer[];
        peers.forEach((peer) => {
          this.addPeer(peer);
        });
        break;

      case "peerDenied":
        this.callbacks.onJoinError?.(deserializedMediaEvent.data);
        break;

      case "sdpOffer":
        this.onOffer(deserializedMediaEvent.data);
        break;

      case "sdpAnswer":
        this.onAnswer(deserializedMediaEvent.data);
        break;

      case "candidate":
        this.onRemoteCandidate(deserializedMediaEvent.data);
        break;

      case "peerJoined":
        peer = deserializedMediaEvent.data.peer;
        console.log("Peer joined");
        if (peer.id != this.id) {
          this.addPeer(peer);
          this.callbacks.onPeerJoined?.(peer);
        }
        break;

      case "peerLeft":
        peer = this.idToPeer.get(deserializedMediaEvent.data.peerId);
        if (peer) {
          this.removePeer(peer);
          this.callbacks.onPeerLeft?.(peer);
        }
        break;

      case "error":
        this.callbacks.onConnectionError?.(deserializedMediaEvent.data.message);
        this.leave();
        break;
    }
  };

  /**
   * Adds track that will be sent to the SFU server.
   * At this moment only one audio and one video track can be added.
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
  public addTrack(track: MediaStreamTrack, stream: MediaStream, trackMetadata: any = {}) {
    this.localTracksWithStreams.push({ track, stream });
    this.localTrackIdToMetadata.set(track.id, trackMetadata);
  }

  /**
   * Replaces a track that is being sent to the SFU server.
   * At the moment this assumes that only one video and one audio track is being sent.
   * @param track - Audio or video track.
   *
   * @example
   * ```ts
   * // setup camera
   * let localStream: MediaStream = new MediaStream();
   * try {
   *   localVideoStream = await navigator.mediaDevices.getUserMedia(
   *     VIDEO_CONSTRAINTS
   *   );
   *   localVideoStream
   *     .getTracks()
   *     .forEach((track) => localStream.addTrack(track));
   * } catch (error) {
   *   console.error("Couldn't get camera permission:", error);
   * }
   *
   * localStream
   *  .getTracks()
   *  .forEach((track) => webrtc.addTrack(track, localStream));
   *
   * // change camera
   * const oldTrackId = localStream.getVideoTracks()[0].id;
   * let videoDeviceId = "abcd-1234";
   * navigator.mediaDevices.getUserMedia({
   *      video: {
   *        ...(VIDEO_CONSTRAINTS as {}),
   *        deviceId: {
   *          exact: videoDeviceId,
   *        },
   *      }
   *   })
   *   .then((stream) => {
   *     let videoTrack = stream.getVideoTracks()[0];
   *     webrtc.replaceTrack(oldTrackId, videoTrack);
   *   })
   *   .catch((error) => {
   *     console.error('Error switching camera', error);
   *   })
   * ```
   */
  public async replaceTrack(oldTrackId: string, newTrack: MediaStreamTrack): Promise<any> {
    const sender = this.connection!.getSenders().find((sender) => {
      return sender!.track!.id === oldTrackId;
    });
    return sender!.replaceTrack(newTrack);
  }

  /**
   * Leaves the room. This function should be called when user leaves the room
   * in a clean way e.g. by clicking a dedicated, custom button `disconnect`.
   * As a result there will be generated one more media event that should be
   * sent to the SFU server. Thanks to it each other peer will be notified
   * that peer left in {@link onPeerLeft},
   */
  public leave = () => {
    let mediaEvent = generateMediaEvent("leave");
    this.sendMediaEvent(mediaEvent);
    this.cleanUp();
  };

  /**
   * Cleans up {@link MembraneWebRTC} instance.
   */
  public cleanUp = () => {
    if (this.connection) {
      this.connection.onicecandidate = null;
      this.connection.ontrack = null;
    }

    this.localTracksWithStreams.forEach(({ track }) => track.stop());
    this.localTracksWithStreams = [];
    this.connection = undefined;
  };

  private sendMediaEvent = (mediaEvent: MediaEvent) => {
    this.callbacks.onSendMediaEvent(serializeMediaEvent(mediaEvent));
  };

  private changeLineToSendOnly = (line: string): string => line == "a=sendrecv" ? "a=sendonly" : line

  private endline = "\r\n"

  private findMid = (splittedTracks: string[]): string[] => {
    const splitted = splittedTracks
    const linesWithMid = splitted?.filter(elem => elem.startsWith("a=mid:"))
    return linesWithMid?.map(line => line.substring(6))
  }

  private midLine = "a=mid:";

  private replaceMid = (splittedTracks: string[], newMids: string[]): string[] => {
    let i = 0;
    return splittedTracks.map(elem => elem.startsWith(this.midLine) ? newMids[i++] : elem)
  }

  private getTransceiverNumbers = (splittedTracks: string[]): string[] =>
    splittedTracks.filter((line) => line.startsWith("m=")).map((line) => line.slice(2, 7));

  private onAnswer = async (answer: RTCSessionDescriptionInit) => {
    console.log(answer)
    if (this.connection) {
      // this.connection.onicecandidate = this.onLocalCandidate();
      // this.connection.ontrack = this.onTrack();
      try { await this?.connection.setRemoteDescription(answer); console.log("Set answer description") }
      catch (err) { console.log(err) }
    } else
      console.log("Fatal error")
  }

  private addTransceiversIfNeeded = (serverTracks: string[], mediaType: string) => {
    const serverMedia = serverTracks.filter(elem => elem === mediaType)
    const recvTransceivers = this.connection!.getTransceivers().filter(elem => elem.currentDirection === "recvonly")
    const mediaTransceivers = recvTransceivers.filter((elem) => elem.receiver.track.kind === mediaType)

    const toAdd = serverMedia.length - mediaTransceivers.length
    const config = mediaType === "audio" ? DEFAULT_TRANSCEIVER_CONFIG.RECV_AUDIO : DEFAULT_TRANSCEIVER_CONFIG.RECV_VIDEO
    if (toAdd > 0)
      for (let i = 0; i < toAdd; i++)
        this.connection?.addTransceiver(mediaType, config)
  }

  private inMids: string[] = []

  private getNewMids = (mids: string[], inMids: string[]): string[] => {
    const usedMids = this.midToPeer.keys()
    const newMidsNeededLength = inMids.filter(mid => !(mid in usedMids)).length
    const midsLength: number = mids.length
    const allMids = Array.from(Array(midsLength).keys()).map(mid => mid.toString()).filter(mid => !(this.inMids.includes(mid)))
    this.inMids = this.inMids.concat(allMids.slice(-newMidsNeededLength))
    return this.inMids.concat(allMids.slice(0, - newMidsNeededLength)).map(mid => "a=mid:" + mid)
  }


  private sdpMugging = (sdpOffer: string, inMids: string[]): string => {
    const splittedOffer = sdpOffer.split(this.endline)
    const mids = this.findMid(splittedOffer)
    const newMids = this.getNewMids(mids, inMids)
    const changedOffer = splittedOffer.map(line => this.changeLineToSendOnly(line))
    return this.replaceMid(changedOffer, newMids).join(this.endline)
  }

  private onOffer = async (offerMedia: RTCSessionDescriptionInit, isSimulcast: Boolean = false) => {
    if (!this.connection) {
      this.connection = new RTCPeerConnection(this.rtcConfig);
      this.connection.onicecandidate = this.onLocalCandidate();
      this.connection.ontrack = this.onTrack();

      let stream = this.localTracksWithStreams[0].stream;

      if (isSimulcast)
        stream.getTracks().forEach((track) => {
          let transceiverConfig: RTCRtpTransceiverInit =
            track.kind === "video"
              ? DEFAULT_TRANSCEIVER_CONFIG.VIDEO
              : DEFAULT_TRANSCEIVER_CONFIG.AUDIO;
          this.connection?.addTransceiver(track, transceiverConfig);
        });
      else {
        this.localTracksWithStreams.forEach(({ track, stream }) => {
          this.connection!.addTrack(track, stream);
        });
      }
    } else {
      // this.connection.createOffer({ iceRestart: true });
      await this.connection.restartIce();
    }

    const serverTracks = offerMedia.sdp !== undefined ? this.getTransceiverNumbers(offerMedia?.sdp.split(this.endline)) : []

    const sendMids: string[] = this.connection.getTransceivers().map(trans => trans.mid!)

    this.addTransceiversIfNeeded(serverTracks, "audio")
    this.addTransceiversIfNeeded(serverTracks, "video")


    try {
      const offer = await this.connection.createOffer();
      offer.sdp = this.sdpMugging(offer.sdp!, sendMids)
      console.log(offer.sdp)
      console.log(offer)
      await this.connection.setLocalDescription(offer);
      // await this.connection.setRemoteDescription(offer);
      // const answer = await this.connection.createAnswer();
      // await this.connection.setLocalDescription(answer);

      const localTrackMidToMetadata = {} as any;

      this.connection.getTransceivers().forEach((transceiver) => {
        const trackId = transceiver.sender.track?.id;
        const mid = transceiver.mid;
        if (trackId && mid) {
          this.midToTrackMetadata.set(mid, this.localTrackIdToMetadata.get(trackId));
          localTrackMidToMetadata[mid] = this.localTrackIdToMetadata.get(trackId);
        }
      });
      let mediaEvent = generateMediaEvent("sdpOffer", {
        sdpOffer: offer,
        midToTrackMetadata: localTrackMidToMetadata,
      });
      this.sendMediaEvent(mediaEvent);
    } catch (error) {
      console.error(error);
    }
  };

  private onRemoteCandidate = (candidate: RTCIceCandidate) => {
    try {
      const iceCandidate = new RTCIceCandidate(candidate);
      if (!this.connection) {
        throw new Error("Received new remote candidate but RTCConnection is undefined");
      }
      this.connection.addIceCandidate(iceCandidate);
    } catch (error) {
      console.error(error);
    }
  };

  private onLocalCandidate = () => {
    return (event: RTCPeerConnectionIceEvent) => {
      if (event.candidate) {
        let mediaEvent = generateMediaEvent("candidate", {
          candidate: event.candidate.candidate,
          sdpMLineIndex: event.candidate.sdpMLineIndex,
        });
        this.sendMediaEvent(mediaEvent);
      }
    };
  };

  private onTrack = () => {
    return (event: RTCTrackEvent) => {
      const [stream] = event.streams;
      const mid = event.transceiver.mid!;

      const peer = this.midToPeer.get(mid)!;

      this.midToStream.set(mid, stream);

      // console.log(mid)
      // console.log(this.midToPeer)
      console.log(event)
      // console.log(stream)

      stream.onremovetrack = (e) => {
        const hasTracks = stream.getTracks().length > 0;

        if (!hasTracks) {
          this.midToStream.delete(mid);
          stream.onremovetrack = null;
        }

        this.callbacks.onTrackRemoved?.({
          peer,
          track: e.track,
          stream,
          mid: event.transceiver.mid!,
          metadata: this.midToTrackMetadata.get(mid),
        });
      };

      this.callbacks.onTrackAdded?.({
        track: event.track,
        peer,
        stream,
        mid: event.transceiver.mid!,
        metadata: this.midToTrackMetadata.get(mid),
      });
    };
  };

  private addPeer = (peer: Peer): void => {
    for (let key in peer.midToTrackMetadata) {
      this.midToPeer.set(key, peer);
      this.midToTrackMetadata.set(key, peer.midToTrackMetadata[key]);
    }
    this.idToPeer.set(peer.id, peer);
  };

  private removePeer = (peer: Peer): void => {
    for (let key in peer.midToTrackMetadata) {
      this.midToPeer.delete(key);
      this.midToTrackMetadata.delete(key);
    }
    this.idToPeer.delete(peer.id);
  };
}
