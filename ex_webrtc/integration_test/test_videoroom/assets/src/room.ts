import { WebRTCEndpoint, Endpoint, TrackContext, Encoding, TrackKind } from "@fishjam-cloud/ts-client";
import { Push, Socket } from "phoenix";
import {
  addVideoElement,
  removeVideoElement,
  setErrorMessage,
  attachStream,
} from "./room_ui";

// export const MEDIA_CONSTRAINTS: MediaStreamConstraints = {
//   video: { width: 640, height: 360, frameRate: 24 },
//   audio: true,
// };

export const LOCAL_ENDPOINT_ID = "local-endpoint";


export type EndpointMetadata = {
  displayName: string;
};

export type TrackMetadata = {
  goodTrack: string;
};


export class Room {
  public endpointId: string | undefined;
  private endpoints: Endpoint<EndpointMetadata, TrackMetadata>[] = [];
  private displayName: string;
  private localStream: MediaStream | undefined;
  public webrtc: WebRTCEndpoint;
  private constraints: MediaStreamConstraints;

  public streams: { [key: string]: MediaStream };
  private removedTracks: string[];

  private socket;
  private webrtcSocketRefs: string[] = [];
  private webrtcChannel;

  public lastPeerMetadata: string | undefined;
  public lastTrackMetadata: string | undefined;

  constructor(contraints: MediaStreamConstraints) {
    this.constraints = contraints;
    this.socket = new Socket("/socket");
    this.socket.connect();
    this.displayName = "someone";
    this.webrtcChannel = this.socket.channel("room");

    this.webrtcChannel.onError(() => {
      this.socketOff();
      window.location.reload();
    });
    this.webrtcChannel.onClose(() => {
      this.socketOff();
      window.location.reload();
    });

    this.webrtcSocketRefs.push(this.socket.onError(this.leave));
    this.webrtcSocketRefs.push(this.socket.onClose(this.leave));

    this.webrtc = new WebRTCEndpoint();
    this.streams = {};
    this.removedTracks = [];

    setInterval(() => {
      console.log("streams", this.streams);
    }, 5000);

    this.webrtc.on("sendMediaEvent", (mediaEvent: string) => {
      this.webrtcChannel.push("mediaEvent", { data: mediaEvent });
    })

    this.webrtc.on("connectionError", (e) => setErrorMessage(e.message));

    this.webrtc.on("connected", async (endpointId: string, otherEndpoints: Endpoint<EndpointMetadata, TrackMetadata>[]) => {
      this.endpointId = endpointId;
      this.endpoints = otherEndpoints;
      this.endpoints.forEach((endpoint) => {
        const displayName = endpoint.metadata?.displayName || "undefined";
        addVideoElement(endpoint.id, displayName, false);
      });
      this.updateParticipantsList();

      for (const track of this.localStream!.getTracks()) {
        await this.webrtc.addTrack(track, { peer: this.displayName, kind: track.kind });
      }
    });
    this.webrtc.on("connectionError", () => { throw `Endpoint denied.` });

    this.webrtc.on("trackReady", (ctx: TrackContext<EndpointMetadata, TrackMetadata>) => {
      this.streams[ctx.stream!.id] = ctx.stream!;

      console.log("trackReady", ctx.trackId, ctx.track!.kind, ctx.stream!.id);
      attachStream(ctx.stream!, ctx.endpoint.id)
    });

    this.webrtc.on("trackRemoved", (ctx) => {
      this.removedTracks.push(ctx.track!.id);

      console.log("trackRemoved", ctx.track!.id, ctx.track!.kind);

      if (ctx.stream?.getTracks()!.every((track) => this.removedTracks.includes(track.id)))
        delete this.streams[ctx.stream!.id];
    });

    this.webrtc.on("endpointAdded", (endpoint: Endpoint<EndpointMetadata, TrackMetadata>) => {
      this.endpoints.push(endpoint);
      this.updateParticipantsList();
      addVideoElement(endpoint.id, endpoint.metadata?.displayName!, false);
    });

    this.webrtc.on("endpointRemoved", (endpoint: Endpoint<EndpointMetadata, TrackMetadata>) => {
      this.endpoints = this.endpoints.filter((e) => e.id !== endpoint.id);
      removeVideoElement(endpoint.id);
      this.updateParticipantsList();
    });

    this.webrtcChannel.on("mediaEvent", (event: any) => {
      // console.log("MediaEvent", event);
      this.webrtc.receiveMediaEvent(event.data);
    }
    );

    this.webrtc.on("endpointUpdated", (endpoint) => {
      this.lastPeerMetadata = endpoint.metadata;
    });

    this.webrtc.on("trackUpdated", (ctx) => {
      this.lastTrackMetadata = ctx.metadata;
    });
  }

  public join = async () => {
    try {
      await this.init();
      this.webrtc.connect({ displayName: this.displayName });
    } catch (error) {
      console.error("Error while joining to the room:", error);
    }
  };

  public updateMetadata = (metadata: any) => {
    this.webrtc.updateEndpointMetadata(metadata);
  };

  public updateTrackMetadata = (metadata: any) => {
    const tracks = this.webrtc.getLocalEndpoint().tracks;
    const trackId = tracks.keys().next().value!;
    this.webrtc.updateTrackMetadata(trackId, metadata);
  }

  public selectPeerSimulcastEncoding = (encoding: Encoding) => {
    const peer = this.endpoints[0]
    const trackIds = Array.from(peer.tracks.keys())

    const remoteTracks = this.webrtc.getRemoteTracks();

    const videoTrackIds = trackIds.filter(trackId => remoteTracks[trackId].track?.kind == "video")
    videoTrackIds.forEach(trackId => this.webrtc.setTargetTrackEncoding(trackId, encoding))
  }

  public disableSimulcastEncoding = (encoding: Encoding) => {
    const trackId = Object.keys(Array.from(this.webrtc.getLocalEndpoint().tracks.values()).filter((track => track.trackKind === "video")))[0];

    console.log("disableSimulcastEncoding trackID", trackId);

    this.webrtc.disableTrackEncoding(trackId, encoding)
  }

  public enableSimulcastEncoding = (encoding: Encoding) => {
    const trackId = Object.keys(Array.from(this.webrtc.getLocalEndpoint().tracks.values()).filter((track => track.trackKind === "video")))[0];

    this.webrtc.enableTrackEncoding(trackId, encoding)
  }

  public getEndpointTrack = (endpointId: string, kind: TrackKind) => {
    const tracksCtxs = Array.from(Object.values(this.webrtc.getRemoteTracks()));
    const trackCtx = tracksCtxs.find(trackCtx => trackCtx.endpoint.id === endpointId && trackCtx.track?.kind === kind);
    return trackCtx?.track;
  }

  public getPeerEncoding = () => { return this.webrtc.getLocalEndpoint().tracks }

  private init = async () => {
    if (this.constraints.audio != false || this.constraints.video != false) {
      try {
        this.localStream = await navigator.mediaDevices.getUserMedia(
          this.constraints
        );
      } catch (error) {
        console.error(error);
        setErrorMessage(
          "Failed to setup video room, make sure to grant camera and microphone permissions"
        );
        throw "error";
      }

      addVideoElement(LOCAL_ENDPOINT_ID, "Me", true);
      attachStream(this.localStream!, LOCAL_ENDPOINT_ID);
    }

    await this.phoenixChannelPushResult(this.webrtcChannel.join());
  };

  public leave = () => {
    this.webrtc.disconnect();
    this.webrtcChannel.leave();
    this.socketOff();
  };

  private socketOff = () => {
    this.socket.off(this.webrtcSocketRefs);
    while (this.webrtcSocketRefs.length > 0) {
      this.webrtcSocketRefs.pop();
    }
  };


  private updateParticipantsList = (): void => {
    const participantsNames = this.endpoints.map((e) => e.metadata?.displayName!);

    if (this.displayName) {
      participantsNames.push(this.displayName);
    }
  };

  private phoenixChannelPushResult = async (push: Push): Promise<any> => {
    return new Promise((resolve, reject) => {
      push
        .receive("ok", (response: any) => resolve(response))
        .receive("error", (response: any) => reject(response));
    });
  };
}


// class Room {

//   constructor(localStream, simulcast) {
//     this.localStream = localStream;
//     this.peers = [];
//     this.socket = new Socket("/socket");
//     this.socket.connect();
//     this.displayName = "local";
//     this.webrtcChannel = this.socket.channel("room");
//     this.videoTrack = null;
//     this.audioTrack = null;
//     this.peerEncoding = "m"
//     this.encodings = ["l", "m", "h"];
//     this.peerMetadata = null;
//     this.trackMetadata = null;
//     this.peerIdToVideoTrack = {};
//     this.selfId = null;
//     this.simulcast = simulcast;
//     this.remoteTracks = new Map();

//     this.webrtcSocketRefs = [];
//     this.webrtcSocketRefs.push(this.socket.onError(this.leave));
//     this.webrtcSocketRefs.push(this.socket.onClose(this.leave));

//     this.webrtc = new WebRTCEndpoint();

//     this.webrtc.on("sendMediaEvent", (mediaEvent) => {
//       this.webrtcChannel.push("mediaEvent", { data: mediaEvent });
//     });

//     this.webrtc.on("connectionError", setErrorMessage);

//     this.webrtc.on("connected", (endpointId, otherEndpoints) => {
//       this.selfId = endpointId;
//       if (this.localStream) {
//         this.localStream
//           .getTracks()
//           .forEach((track) => this.addTrack(track))
//       }

//       this.peers = otherEndpoints.filter((endpoint) => endpoint.type === "webrtc");
//       this.peers.forEach((peer) => {
//         addVideoElement(peer.id);
//       });
//       this.updateParticipantsList();
//     });

//     this.webrtc.on("trackReady", (ctx) => {
//       const video = document.getElementById(ctx.endpoint.id);

//       video.srcObject = ctx.stream;
//       this.remoteTracks.set(ctx.trackId, ctx);

//       if (ctx.track.kind === "video") {
//         this.peerIdToVideoTrack[ctx.endpoint.id] = ctx.track;
//       }
//     });

//     this.webrtc.on("trackAdded", (ctx) => {
//       ctx.on("encodingChanged", (trackCtx) => {
//         this.peerEncoding = trackCtx.encoding;
//       });
//     });

//     this.webrtc.on("endpointAdded", (endpoint) => {
//       if (endpoint.type !== "webrtc") return;
//       this.peers.push(endpoint);
//       this.updateParticipantsList();
//       addVideoElement(endpoint.id, endpoint.metadata.displayName, false);
//     });

//     this.webrtc.on("endpointRemoved", (endpoint) => {
//       const peer = this.peers.find((peer) => peer.id === endpoint.id);
//       if (!peer) return;
//       this.peers = this.peers.filter((p) => p.id !== peer.id);
//       removeVideoElement(peer.id);
//       this.updateParticipantsList();
//     });

//     this.webrtc.on("endpointUpdated", (endpoint) => {
//       this.peerMetadata = endpoint.metadata;
//     });

//     this.webrtc.on("trackUpdated", (ctx) => {
//       console.log("Track updated")
//       this.trackMetadata = ctx.metadata;
//     });

//     this.webrtcChannel.on("mediaEvent", (event) => this.webrtc.receiveMediaEvent(event.data));
//   }

//   addTrack = async (track) => {
//     let trackId = !this.simulcast || track.kind == "audio"
//       ? this.webrtc.addTrack(track, this.localStream)
//       : this.webrtc.addTrack(
//         track,
//         this.localStream,
//         {},
//         { enabled: true, activeEncodings: this.encodings },
//         new Map([
//           ["h", 1500],
//           ["m", 500],
//           ["l", 100],
//         ]))

//     trackId = await trackId;

//     if (track.kind == "audio") this.audioTrack = [trackId, track];
//     else this.videoTrack = [trackId, track];
//   }

//   init = async () => {
//     await this.phoenixChannelPushResult(this.webrtcChannel.join());
//   };

//   join = () => {
//     this.webrtc.connect({ displayName: this.displayName });
//   };

//   leave = () => {
//     this.webrtc.disconnect();
//     this.webrtcChannel.leave();
//     this.socket.off(this.webrtcSocketRefs);
//     while (this.webrtcSocketRefs.length > 0) {
//       this.webrtcSocketRefs.pop();
//     }
//   };

//   updateParticipantsList = () => {
//     const participantsNames = this.peers.map((p) => p.metadata.displayName);

//     if (this.displayName) {
//       participantsNames.push(this.displayName);
//     }
//   };

//   phoenixChannelPushResult = async (push) => {
//     return new Promise((resolve, reject) => {
//       push
//         .receive("ok", (response) => resolve(response))
//         .receive("error", (response) => reject(response));
//     });
//   };

//   disableSimulcastEncoding = (encoding) => {
//     this.webrtc.disableTrackEncoding(this.videoTrack[0], encoding)
//   }

//   enableSimulcastEncoding = (encoding) => {
//     this.webrtc.enableTrackEncoding(this.videoTrack[0], encoding)
//   }

//   selectPeerSimulcastEncoding = (encoding) => {
//     const peer = this.peers[0]
//     const trackIds = Array.from(peer.tracks.keys())
//     const videoTrackIds = trackIds.filter(trackId => this.remoteTracks.get(trackId).track.kind == "video")
//     videoTrackIds.forEach(trackId => this.webrtc.setTargetTrackEncoding(trackId, encoding))
//   }

//   getPeerEncoding = () => { return this.peerEncoding }

//   updateMetadata = () => this.webrtc.updateEndpointMetadata("test")
//   updateTrackMetadata = () => this.webrtc.updateTrackMetadata(this.videoTrack[0], "trackMetadata")
// }

// export default Room;
