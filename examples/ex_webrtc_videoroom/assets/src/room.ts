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
import {
  WebRTCEndpoint,
  Endpoint,
  TrackContext,
} from "@fishjam-cloud/ts-client";
import { Push, Socket } from "phoenix";
import { parse } from "query-string";

export type EndpointMetadata = {
  displayName: string;
};

export type TrackMetadata = {
  goodTrack: string;
};

export class Room {
  private endpoints: Endpoint<EndpointMetadata, TrackMetadata>[] = [];
  private displayName: string;
  private localStream: MediaStream | undefined;
  private webrtc: WebRTCEndpoint;

  private socket;
  private webrtcSocketRefs: string[] = [];
  private webrtcChannel;

  constructor() {
    this.socket = new Socket("/socket");
    this.socket.connect();
    this.displayName = this.parseUrl() || "undefined";
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

    this.webrtc = new WebRTCEndpoint();

    this.webrtc.on("sendMediaEvent", (mediaEvent: string) => {
      this.webrtcChannel.push("mediaEvent", { data: mediaEvent });
    })

    this.webrtc.on("connectionError", (e) => setErrorMessage(e.message));

    this.webrtc.on("connected", async (endpointId: string, otherEndpoints: Endpoint<EndpointMetadata, TrackMetadata>[]) => {
      this.endpoints = otherEndpoints;
      this.endpoints.forEach((endpoint) => {
        const displayName = endpoint.metadata?.displayName || "undefined";
        addVideoElement(endpoint.id, displayName, false);
      });
      this.updateParticipantsList();

      for (const track of this.localStream!.getTracks()) {
        console.log("addingTrack...");
        await this.webrtc.addTrack(track, { peer: this.displayName, kind: track.kind });
        console.log("room addedTrack", track)
      }
    });
    this.webrtc.on("connectionError", () => { throw `Endpoint denied.` });

    this.webrtc.on("trackReady", (ctx: TrackContext<EndpointMetadata, TrackMetadata>) => {
      console.log("trackReady", ctx);
      attachStream(ctx.stream!, ctx.endpoint.id)
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
      console.log("MediaEvent", event);
      this.webrtc.receiveMediaEvent(event.data);
    }
    );
  }

  public join = async () => {
    try {
      await this.init();
      setupDisconnectButton(() => {
        this.leave();
        window.location.replace("");
      });
      this.webrtc.connect({ displayName: this.displayName });
      console.log("Connecting");
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

  private parseUrl = (): string | undefined => {
    const { display_name: displayName } = parse(document.location.search);

    // remove query params without reloading the page
    window.history.replaceState(null, "", window.location.pathname);

    return displayName as string | undefined;
  };

  private updateParticipantsList = (): void => {
    const participantsNames = this.endpoints.map((e) => e.metadata?.displayName!);

    if (this.displayName) {
      participantsNames.push(this.displayName);
    }

    setParticipantsList(participantsNames);
  };

  private phoenixChannelPushResult = async (push: Push): Promise<any> => {
    return new Promise((resolve, reject) => {
      push
        .receive("ok", (response: any) => resolve(response))
        .receive("error", (response: any) => reject(response));
    });
  };
}
