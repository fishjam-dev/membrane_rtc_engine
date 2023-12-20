import { Push, Socket } from "phoenix";
import { setErrorMessage, setPlayerInfo, setPreview } from "./ui";
import { WebRTCEndpoint } from "@jellyfish-dev/membrane-webrtc-js";

const awaitPhoenixPush = async (push: Push): Promise<any> => {
  return new Promise((resolve, reject) => {
    push
      .receive("ok", (response: any) => resolve(response))
      .receive("error", (response: any) => reject(response));
  });
};

export const AUDIO_CONSTRAINTS: MediaStreamConstraints = {
  audio: true,
  video: false,
};

export const VIDEO_CONSTRAINTS: MediaStreamConstraints = {
  audio: false,
  video: { width: 1280, height: 720, frameRate: 24 },
};

export const LOCAL_PEER_ID = "local-peer";

export const setup = async () => {
  const callbacksRefs: string[] = [];
  const socket = new Socket("/socket");
  socket.connect();

  let localAudioStream: MediaStream | null = null;
  let localVideoStream: MediaStream | null = null;
  let localStream: MediaStream = new MediaStream();

  try {
    localAudioStream = await navigator.mediaDevices.getUserMedia(
      AUDIO_CONSTRAINTS
    );
    localAudioStream
      .getTracks()
      .forEach((track) => localStream.addTrack(track));
  } catch (error) {
    console.error("Couldn't get microphone permission:", error);
  }

  try {
    localVideoStream = await navigator.mediaDevices.getUserMedia(
      VIDEO_CONSTRAINTS
    );
    localVideoStream.getTracks().forEach((track) => {
      localStream.addTrack(track);
    });
  } catch (error) {
    console.error("Couldn't get camera permission:", error);
  }

  setPreview(localStream);

  const webrtcChannel = socket.channel("stream");

  const socketOff = () => {
    socket.off(callbacksRefs);
    while (callbacksRefs.length > 0) {
      callbacksRefs.pop();
    }
  };

  const leave = () => {
    webrtc.disconnect();
    webrtcChannel.leave();
    socketOff();
  };

  webrtcChannel.onError((error: any) => {
    setErrorMessage(error);
    socketOff();
    window.location.reload();
  });
  webrtcChannel.onClose(() => {
    socketOff();
    window.location.reload();
  });

  window.onbeforeunload = (event) => {
    leave();
  };

  callbacksRefs.push(socket.onError(leave));
  callbacksRefs.push(socket.onClose(leave));

  const webrtc = new WebRTCEndpoint();

  webrtc.on("sendMediaEvent", (mediaEvent: string) => {
    webrtcChannel.push("mediaEvent", { data: mediaEvent })
  })

  webrtc.on("connected", () => {
    localStream
      .getTracks()
      .forEach((track) => webrtc.addTrack(track, localStream, {}));
  });

  await awaitPhoenixPush(webrtcChannel.join());

  webrtc.connect({
    displayName: "It's me, Mario!",
  });

  webrtcChannel.on("mediaEvent", (event) =>
    webrtc.receiveMediaEvent(event.data)
  );
  webrtcChannel.on("playlistPlayable", ({ playlistId }) => {
    setPlayerInfo(playlistId);
  });
};
