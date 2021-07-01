import { Channel, Push, Socket } from "phoenix";

import { MediaEvent, MediaCallbacks } from "./membraneWebRTC";

export const phoenixChannelPushResult = async (push: Push): Promise<any> => {
  return new Promise((resolve, reject) => {
    push
      .receive("ok", (response: any) => resolve(response))
      .receive("error", (response: any) => reject(response));
  });
};

export function getChannelId(
  type: "participant" | "screensharing",
  roomId: string
) {
  if (type === "participant") {
    return `room:${roomId}`;
  } else {
    return `room:screensharing:${roomId}`;
  }
}

export function getMediaCallbacksFromPhoenixChannel(
  channel: Channel
): MediaCallbacks {
  return {
    push: (event: MediaEvent) => channel.push("mediaEvent", event),
    pushResult: async (event: MediaEvent) =>
      phoenixChannelPushResult(channel.push("mediaEvent", event)),
  };
}
