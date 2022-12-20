import { MembraneWebRTC } from "@membraneframework/membrane-webrtc-js";

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
    this.selfId = null;
    this.simulcast = simulcast;
    this.remoteTracks = new Map();

    this.webrtcSocketRefs = [];
    this.webrtcSocketRefs.push(this.socket.onError(this.leave));
    this.webrtcSocketRefs.push(this.socket.onClose(this.leave));

    this.webrtc = new MembraneWebRTC({
      callbacks: {
        onSendMediaEvent: (mediaEvent) => {
          this.webrtcChannel.push("mediaEvent", { data: mediaEvent });
        },
        onConnectionError: setErrorMessage,
        onJoinSuccess: (peerId, peersInRoom) => {
          this.selfId = peerId
          if (this.localStream) {
            this.localStream
              .getTracks()
              .forEach((track) => this.addTrack(track))
          }

          this.peers = peersInRoom;
          this.peers.forEach((peer) => {
            addVideoElement(peer.id);
          });
          this.updateParticipantsList();
        },
        onJoinError: (metadata) => {
          throw `Peer denied.`;
        },
        onTrackReady: (ctx) => {
          const video = document.getElementById(ctx.peer.id);

          video.srcObject = ctx.stream;
          this.remoteTracks.set(ctx.trackId, ctx);
        },
        onTrackAdded: (ctx) => {
          console.log(this.selfId, " track added ", ctx.trackId)
        },
        onTrackRemoved: (ctx) => {
          this.remoteTracks.delete(ctx.trackId);
        },
        onPeerJoined: (peer) => {
          this.peers.push(peer);
          this.updateParticipantsList();
          addVideoElement(peer.id, peer.metadata.displayName, false);
        },
        onPeerLeft: (peer) => {
          this.peers = this.peers.filter((p) => p.id !== peer.id);
          removeVideoElement(peer.id);
          this.updateParticipantsList();
        },
        onPeerUpdated: (ctx) => { this.peerMetadata = ctx.metadata },
        onTrackUpdated: (ctx) => { this.trackMetadata = ctx.metadata },
        onTrackEncodingChanged: (peerId, trackId, encoding) => {
          console.log(this.selfId, "received info that ", peerId, "changed encoding to ", encoding)
          this.peerEncoding = encoding
        }

      },
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
        { enabled: true, active_encodings: this.encodings })


    if (track.kind == "audio") this.audioTrack = [trackId, track];
    else this.videoTrack = [trackId, track];
  }

  init = async () => {
    await this.phoenixChannelPushResult(this.webrtcChannel.join());
  };

  join = () => {
    this.webrtc.join({ displayName: this.displayName });
  };

  leave = () => {
    this.webrtc.leave();
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

  updateMetadata = () => this.webrtc.updatePeerMetadata("test")
  updateTrackMetadata = () => this.webrtc.updateTrackMetadata(this.videoTrack[0], "trackMetadata")
}

export default Room;
