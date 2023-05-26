import { WebRTCEndpoint } from "@jellyfish-dev/membrane-webrtc-js";

import { Socket } from "phoenix";

const videos = document.querySelector("#videos");

function addVideoElement(id) {
  const video = document.createElement("video");
  video.id = id;
  video.autoplay = true;
  video.playsInline = true;
  videos.appendChild(video);
}

function removeVideoElement(id) {
  const video = document.getElementById(id);
  videos.removeChild(video);
}

function setErrorMessage(error) {
  console.error(error);
}

class Room {
  constructor(localStream, simulcast) {
    this.localStream = localStream;
    this.peers = [];
    this.socket = new Socket("/socket");
    this.socket.connect();
    this.displayName = "local";
    this.endpointChannel = this.socket.channel("room");
    this.videoTrack = null;
    this.audioTrack = null;
    this.peerEncoding = "m"
    this.encodings = ["l", "m", "h"];
    this.peerMetadata = null;
    this.trackMetadata = null;
    this.selfId = null;
    this.simulcast = simulcast;
    this.remoteTracks = new Map();

    this.endpointSocketRefs = [];
    this.endpointSocketRefs.push(this.socket.onError(this.leave));
    this.endpointSocketRefs.push(this.socket.onClose(this.leave));

    this.endpoint = new WebRTCEndpoint();

    this.endpoint.on("sendMediaEvent", (mediaEvent) => {
      this.endpointChannel.push("mediaEvent", { data: mediaEvent });
    });

    this.endpoint.on("connectionError", setErrorMessage);

    this.endpoint.on("connected", (endpointId, otherEndpoints) => {
      this.selfId = endpointId;
      if (this.localStream) {
        this.localStream
          .getTracks()
          .forEach((track) => this.addTrack(track))
      }

      this.peers = otherEndpoints.filter((endpoint) => endpoint.type === "webrtc");
      this.peers.forEach((peer) => {
        addVideoElement(peer.id);
      });
      this.updateParticipantsList();
    });

    this.endpoint.on("trackReady", (ctx) => {
      const video = document.getElementById(ctx.endpoint.id);

      video.srcObject = ctx.stream;
      this.remoteTracks.set(ctx.trackId, ctx);
    });

    this.endpoint.on("trackAdded", (ctx) => {
      console.log(this.selfId, " track added ", ctx.trackId)
      ctx.on("encodingChanged", (trackCtx) => {
        console.log(this.selfId, "received info that ", trackCtx.endpoint.id, "changed encoding to ", trackCtx.encoding);
        this.peerEncoding = trackCtx.encoding;
      });
    });

    this.endpoint.on("trackRemoved", (ctx) => {
      this.remoteTracks.delete(ctx.trackId);
    });

    this.endpoint.on("endpointAdded", (endpoint) => {
      if (endpoint.type !== "webrtc") return;
      this.peers.push(endpoint);
      this.updateParticipantsList();
      addVideoElement(endpoint.id, endpoint.metadata.displayName, false);
    });

    this.endpoint.on("endpointRemoved", (endpoint) => {
      const peer = this.peers.find((peer) => peer.id === endpoint.id);
      if (!peer) return;
      this.peers = this.peers.filter((p) => p.id !== peer.id);
      removeVideoElement(peer.id);
      this.updateParticipantsList();
    });

    this.endpoint.on("endpointUpdated", (endpoint) => {
      this.peerMetadata = endpoint.metadata;
    });

    this.endpoint.on("trackUpdated", (ctx) => {
      this.trackMetadata = ctx.metadata;
    });

    this.endpointChannel.on("mediaEvent", (event) => this.endpoint.receiveMediaEvent(event.data));
  }

  addTrack = (track) => {
    let trackId = !this.simulcast || track.kind == "audio"
      ? this.endpoint.addTrack(track, this.localStream)
      : this.endpoint.addTrack(
        track,
        this.localStream,
        {},
        { enabled: true, active_encodings: this.encodings },
        new Map([
          ["h", 1500],
          ["m", 500],
          ["l", 100],
        ]))


    if (track.kind == "audio") this.audioTrack = [trackId, track];
    else this.videoTrack = [trackId, track];
  }

  init = async () => {
    await this.phoenixChannelPushResult(this.endpointChannel.join());
  };

  join = () => {
    this.endpoint.connect({ displayName: this.displayName });
  };

  leave = () => {
    this.endpoint.leave();
    this.endpointChannel.leave();
    this.socket.off(this.endpointSocketRefs);
    while (this.endpointSocketRefs.length > 0) {
      this.endpointSocketRefs.pop();
    }
  };

  updateParticipantsList = () => {
    const participantsNames = this.peers.map((p) => p.metadata.displayName);

    if (this.displayName) {
      participantsNames.push(this.displayName);
    }
  };

  phoenixChannelPushResult = async (push) => {
    return new Promise((resolve, reject) => {
      push
        .receive("ok", (response) => resolve(response))
        .receive("error", (response) => reject(response));
    });
  };

  disableSimulcastEncoding = (encoding) => {
    this.endpoint.disableTrackEncoding(this.videoTrack[0], encoding)
  }

  enableSimulcastEncoding = (encoding) => {
    this.endpoint.enableTrackEncoding(this.videoTrack[0], encoding)
  }

  selectPeerSimulcastEncoding = (encoding) => {
    const peer = this.peers[0]
    const trackIds = Array.from(peer.trackIdToMetadata.keys())
    const videoTrackIds = trackIds.filter(trackId => this.remoteTracks.get(trackId).track.kind == "video")
    videoTrackIds.forEach(trackId => this.endpoint.setTargetTrackEncoding(trackId, encoding))
  }

  getPeerEncoding = () => { return this.peerEncoding }

  updateMetadata = () => this.endpoint.updateEndpointMetadata("test")
  updateTrackMetadata = () => this.endpoint.updateTrackMetadata(this.videoTrack[0], "trackMetadata")
}

export default Room;
