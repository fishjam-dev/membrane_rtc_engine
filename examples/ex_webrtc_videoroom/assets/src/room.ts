import { MEDIA_CONSTRAINTS, LOCAL_ENDPOINT_ID } from "./consts";
import {
  addVideoElement,
  getRoomId,
  removeVideoElement,
  setErrorMessage,
  setParticipantsList,
  attachStream,
  setupDisconnectButton,
} from "./room_ui";
import { Push, Socket } from "phoenix";
import { parse } from "query-string";

const pcConfig = { iceServers: [{ urls: 'stun:stun.l.google.com:19302' }] };
export class Room {
  // private endpoints: Endpoint[] = [];
  private displayName: string;
  private localStream: MediaStream | undefined;
  private pc: RTCPeerConnection;

  private socket;
  private webrtcSocketRefs: string[] = [];
  private webrtcChannel;

  private localTracksAdded = false;

  constructor() {
    this.socket = new Socket("/socket");
    this.socket.connect();
    this.displayName = this.parseUrl();
    this.webrtcChannel = this.socket.channel(`room:${getRoomId()}`);

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

    this.pc = new RTCPeerConnection(pcConfig);

    this.pc.onicegatheringstatechange = () =>
      console.log('Gathering state change: ' + this.pc.iceGatheringState);
    this.pc.onconnectionstatechange = () =>
      console.log('Connection state change: ' + this.pc.connectionState);
    this.pc.onicecandidate = (event) => {
      if (event.candidate == null) {
        console.log('Gathering candidates complete');
        return;
      }

      const candidate = JSON.stringify(event.candidate);
      console.log('Sending ICE candidate: ' + candidate);
      this.sendMediaEvent("custom", { "type": "candidate", "data": candidate });
    };

    this.webrtcChannel.on("mediaEvent", async (payload) => {
      console.log("Media event", payload)
      const { "type": type, "data": data } = JSON.parse(payload.data);

      switch (type) {
        case "custom":
          switch (data.type) {
            case "sdpOffer":
              const offer = data.data;
              this.handle_offer(offer);

            case "candidate":
              const candidate = data.data;
              this.pc.addIceCandidate(candidate);
              console.log('Received ICE candidate: ' + candidate);
          }
      }
    });

    this.pc.ontrack = (event) => {
      console.log("ontrack", event.track.kind, event);

      const stream = event.streams[0];
      const peerId = stream.id;

      if (event.track.kind == 'video') {
        addVideoElement(peerId, "video", false);
        attachStream(stream, peerId);
      }
      else {
        // addVideoElement(peerId, "video", false);
        addVideoElement(peerId, "audio", false);
        attachStream(stream, peerId);
        console.log("nice cock")
      }
    };
  }

  public join = async () => {
    try {
      await this.init();
      setupDisconnectButton(() => {
        this.leave();
        window.location.replace("");
      });

      this.sendMediaEvent("connect", this.displayName);

      console.log("Joined webrtcChannel");

    } catch (error) {
      console.error("Error while joining to the room:", error);
    }
  };

  private init = async () => {
    try {
      this.localStream = await navigator.mediaDevices.getUserMedia(
        MEDIA_CONSTRAINTS
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

    await this.phoenixChannelPushResult(this.webrtcChannel.join());
  };

  private leave = () => {
    this.pc.close();
    this.webrtcChannel.leave();
    this.socketOff();
  };

  private socketOff = () => {
    this.socket.off(this.webrtcSocketRefs);
    while (this.webrtcSocketRefs.length > 0) {
      this.webrtcSocketRefs.pop();
    }
  };

  private parseUrl = (): string => {
    const { display_name: displayName } = parse(document.location.search);

    // remove query params without reloading the page
    window.history.replaceState(null, "", window.location.pathname);

    return displayName as string;
  };

  private sendMediaEvent = (type: string, data: any) => {
    const mediaEvent = JSON.stringify({ type, data })

    this.webrtcChannel.push("mediaEvent", mediaEvent);
  }

  // private updateParticipantsList = (): void => {
  //   const participantsNames = this.endpoints.map((e) => e.metadata?.displayName!);

  //   if (this.displayName) {
  //     participantsNames.push(this.displayName);
  //   }

  //   setParticipantsList(participantsNames);
  // };

  private phoenixChannelPushResult = async (push: Push): Promise<any> => {
    return new Promise((resolve, reject) => {
      push
        .receive("ok", (response: any) => resolve(response))
        .receive("error", (response: any) => reject(response));
    });
  };

  private handle_offer = async (offer: RTCSessionDescriptionInit) => {
    console.log("received offer", offer);
    await this.pc.setRemoteDescription(offer);

    if (!this.localTracksAdded) {
      console.log('Adding local tracks to peer connection');
      this.localStream!.getTracks().forEach((track) => this.pc.addTrack(track));
      this.localTracksAdded = true;

      console.log("added tracks");
    }

    const sdpAnswer = await this.pc.createAnswer();
    await this.pc.setLocalDescription(sdpAnswer);

    const answer = this.pc.localDescription;
    console.log("SDP offer applied, forwarding SDP answer", sdpAnswer);
    this.sendMediaEvent("custom", { "type": "sdpAnswer", "data": answer });
  }
}
