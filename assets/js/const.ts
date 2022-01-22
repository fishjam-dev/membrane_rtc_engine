// const TEMPORAL_LAYERS_COUNT = 2;

export const simulcastConfig: RTCRtpTransceiverInit = {
  direction: "sendonly",
  sendEncodings: [
    {
      rid: "h",
      active: true,
      // maxBitrate: 4_000_000,
      // scalabilityMode: "L1T" + TEMPORAL_LAYERS_COUNT,
    },
    {
      rid: "l",
      active: true,
      // maxBitrate: 4_000_000,
      scaleResolutionDownBy: 4,
      //   scalabilityMode: "L1T" + TEMPORAL_LAYERS_COUNT,
    },
  ],
};
