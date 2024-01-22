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
    this.webrtcChannel = this.socket.channel("room");
    this.videoTrack = null;
    this.audioTrack = null;
    this.peerEncoding = "m"
    this.encodings = ["l", "m", "h"];
    this.peerMetadata = null;
    this.trackMetadata = null;
    this.peerIdToVideoTrack = {};
    this.selfId = null;
    this.simulcast = simulcast;
    this.remoteTracks = new Map();

    this.webrtcSocketRefs = [];
    this.webrtcSocketRefs.push(this.socket.onError(this.leave));
    this.webrtcSocketRefs.push(this.socket.onClose(this.leave));

    this.webrtc = new WebRTCEndpoint();

    this.webrtc.on("sendMediaEvent", (mediaEvent) => {
      this.webrtcChannel.push("mediaEvent", { data: mediaEvent });
    });

    this.webrtc.on("connectionError", setErrorMessage);

    this.webrtc.on("connected", (endpointId, otherEndpoints) => {
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

    this.webrtc.on("trackReady", (ctx) => {
      const video = document.getElementById(ctx.endpoint.id);

      video.srcObject = ctx.stream;
      this.remoteTracks.set(ctx.trackId, ctx);

      if (ctx.track.kind === "video") {
        this.peerIdToVideoTrack[ctx.endpoint.id] = ctx.track;
      }
    });

    this.webrtc.on("trackAdded", (ctx) => {
      console.log(this.selfId, " track added ", ctx.trackId)
      ctx.on("encodingChanged", (trackCtx) => {
        console.log(this.selfId, "received info that ", trackCtx.endpoint.id, "changed encoding to ", trackCtx.encoding);
        this.peerEncoding = trackCtx.encoding;
      });
    });

    this.webrtc.on("trackRemoved", (ctx) => {
      this.remoteTracks.delete(ctx.trackId);
    });

    this.webrtc.on("endpointAdded", (endpoint) => {
      if (endpoint.type !== "webrtc") return;
      this.peers.push(endpoint);
      this.updateParticipantsList();
      addVideoElement(endpoint.id, endpoint.metadata.displayName, false);
    });

    this.webrtc.on("endpointRemoved", (endpoint) => {
      const peer = this.peers.find((peer) => peer.id === endpoint.id);
      if (!peer) return;
      this.peers = this.peers.filter((p) => p.id !== peer.id);
      removeVideoElement(peer.id);
      this.updateParticipantsList();
    });

    this.webrtc.on("endpointUpdated", (endpoint) => {
      this.peerMetadata = endpoint.metadata;
    });

    this.webrtc.on("trackUpdated", (ctx) => {
      this.trackMetadata = ctx.metadata;
    });

    this.webrtcChannel.on("mediaEvent", (event) => this.webrtc.receiveMediaEvent(event.data));
  }

  addTrack = (track) => {
    let trackId = !this.simulcast || track.kind == "audio"
      ? this.webrtc.addTrack(track, this.localStream)
      : this.webrtc.addTrack(
        track,
        this.localStream,
        {},
        { enabled: true, activeEncodings: this.encodings },
        new Map([
          ["h", 1500],
          ["m", 500],
          ["l", 100],
        ]))


    if (track.kind == "audio") this.audioTrack = [trackId, track];
    else this.videoTrack = [trackId, track];
  }

  init = async () => {
    await this.phoenixChannelPushResult(this.webrtcChannel.join());
  };

  join = () => {
    this.webrtc.connect({ displayName: this.displayName });
  };

  leave = () => {
    this.webrtc.disconnect();
    this.webrtcChannel.leave();
    this.socket.off(this.webrtcSocketRefs);
    while (this.webrtcSocketRefs.length > 0) {
      this.webrtcSocketRefs.pop();
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
    this.webrtc.disableTrackEncoding(this.videoTrack[0], encoding)
  }

  enableSimulcastEncoding = (encoding) => {
    this.webrtc.enableTrackEncoding(this.videoTrack[0], encoding)
  }

  selectPeerSimulcastEncoding = (encoding) => {
    const peer = this.peers[0]
    const trackIds = Array.from(peer.trackIdToMetadata.keys())
    const videoTrackIds = trackIds.filter(trackId => this.remoteTracks.get(trackId).track.kind == "video")
    videoTrackIds.forEach(trackId => this.webrtc.setTargetTrackEncoding(trackId, encoding))
  }

  getPeerEncoding = () => { return this.peerEncoding }

  updateMetadata = () => this.webrtc.updateEndpointMetadata("test")
  updateTrackMetadata = () => this.webrtc.updateTrackMetadata(this.videoTrack[0], "trackMetadata")
}

export default Room;
