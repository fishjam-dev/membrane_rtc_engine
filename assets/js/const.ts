const TEMPORAL_LAYERS_COUNT = 2;

export const simulcastConfig: RTCRtpTransceiverInit = {
  direction: "sendonly",
  sendEncodings: [
    {
      rid: "h",
      active: true,
      maxBitrate: 1_200_000,
      // From 720p to 360p
      scaleResolutionDownBy: 2,
      //   scalabilityMode: "L1T" + TEMPORAL_LAYERS_COUNT,
    },
    {
      rid: "l",
      active: true,
      maxBitrate: 100_000,
      // From 720p to 180p
      scaleResolutionDownBy: 4,
      //   scalabilityMode: "L1T" + TEMPORAL_LAYERS_COUNT,
    },
  ],
};
