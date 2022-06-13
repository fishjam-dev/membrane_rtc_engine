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
    this.peerTracks = [];
    this.peerEncoding = "m"
    // this.encodings = ["l", "m", "h"]
    this.encodings = ["l", "m"]

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
          if (this.localStream) {
            this.localStream
              .getTracks()
              .forEach((track) => {
                let trackId = !simulcast || track.kind == "audio"
                  ? this.webrtc.addTrack(track, this.localStream)
                  : this.webrtc.addTrack(
                    track,
                    this.localStream,
                    {},
                    { enabled: true, active_encodings: this.encodings })
                console.log(trackId);

                if (track.kind == "audio") this.audioTrack = [trackId, track];
                else this.videoTrack = [trackId, track];
              })
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
        onTrackReady: ({ stream, peer, metadata }) => {
          const video = document.getElementById(peer.id);

          video.srcObject = stream;
        },
        onTrackAdded: (ctx) => {
          if (simulcast) this.peerTracks.push(ctx.trackId)
        },
        onTrackRemoved: (ctx) => { },
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
        onPeerUpdated: (ctx) => { },
        onTrackEncodingChanged: (peerId, trackId, encoding) => { this.peerEncoding = encoding }

      },
    });

    this.webrtcChannel.on("mediaEvent", (event) => this.webrtc.receiveMediaEvent(event.data));
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

  changePeerSimulcastEncoding = (encdoing) => {
    this.peerTracks.forEach(trackId => this.webrtc.selectTrackEncoding(this.peers[0].id, trackId, encdoing))
  }

  getPeerEncoding = () => { return this.peerEncoding }

}

export default Room;
