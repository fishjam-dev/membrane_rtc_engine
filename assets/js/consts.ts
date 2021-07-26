
const TEMPORAL_LAYERS_COUNT = 2;

interface DefaultTransceiverConfigInterface {
    AUDIO: RTCRtpTransceiverInit,
    VIDEO: RTCRtpTransceiverInit,
    RECV_VIDEO: RTCRtpTransceiverInit
    RECV_AUDIO: RTCRtpTransceiverInit
}

export const DEFAULT_TRANSCEIVER_CONFIG: DefaultTransceiverConfigInterface = {
    AUDIO: {
      direction: "sendonly"
    },
    RECV_AUDIO: {
      direction: "recvonly"
    },
    RECV_VIDEO: {
      direction: "recvonly",
    },
    VIDEO: {
      direction: "sendonly",
      sendEncodings: [
        {
          rid: "h",
          active: true,
          maxBitrate: 1_000_000,
          // From 720p to 576p
          scaleResolutionDownBy: 1.25,
        //   scalabilityMode: "L1T" + TEMPORAL_LAYERS_COUNT,
        },
        {
          rid: "m",
          active: true,
          maxBitrate: 600_000,
          // From 720p to 360p
          scaleResolutionDownBy: 2,
        //   scalabilityMode: "L1T" + TEMPORAL_LAYERS_COUNT,
        },
        {
          rid: "l",
          active: true,
          maxBitrate: 200_000,
          // From 720p to 180p
          scaleResolutionDownBy: 4,
        //   scalabilityMode: "L1T" + TEMPORAL_LAYERS_COUNT,
        }
      ],
    },
  };

