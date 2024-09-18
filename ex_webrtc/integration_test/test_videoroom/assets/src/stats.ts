// check interval is used to check if for given time interval any new packets/frames
// have been processed, it may happen that track was processing media before

import { Endpoint, Encoding } from "@fishjam-cloud/ts-client";

import { Room, EndpointMetadata, TrackMetadata } from "./room";
import { EncodingStats } from "./types";

// but somehow it stopped therefore we need to check deltas instead of
const checkInterval = 200;

export function detectBrowser() {
  if ("InstallTrigger" in window) return "firefox";
  return "chrome";
}

async function sleep(interval: number) {
  return new Promise((resolve, _reject) => {
    setTimeout(resolve, interval);
  });
}

// searches through RTCPStats entries iterator and tries
// to find an entry with a key complying with given prefix
//
// works only for chrome...
const extractStatEntry = (stats: RTCStatsReport, name: string, mediaType: string) => {
  for (let [key, value] of stats.entries()) {
    if (mediaType === "audio") {
      console.log(value)
    }
    if (value.type === name && value.mediaType === mediaType) {
      return value;
    }
  };

  return undefined;
};

async function isVideoPlaying(peerConnection: RTCPeerConnection, videoTrack: MediaStreamTrack) {
  const videoFramedDecoded = async (track: MediaStreamTrack) => {
    if (!track) return -1;

    const videoStats = await peerConnection.getStats(track);
    const inboundVideoStats: RTCInboundRtpStreamStats = extractStatEntry(videoStats, "inbound-rtp", "video")!;

    console.log("inboundVideoStats", inboundVideoStats, inboundVideoStats?.framesDecoded);

    return inboundVideoStats?.framesDecoded || -1;
  };

  const videoFramesStart = await videoFramedDecoded(videoTrack);
  await sleep(checkInterval);
  const videoFramesEnd = await videoFramedDecoded(videoTrack);

  console.log("startendvideo", videoFramesStart, videoFramesEnd);

  return videoFramesStart >= 0 && videoFramesEnd > videoFramesStart;
}

async function isAudioPlaying(peerConnection: RTCPeerConnection, audioTrack: MediaStreamTrack) {
  const audioTotalEnergy = async (track: MediaStreamTrack) => {
    if (!track) return -1;

    const audioStats = await peerConnection.getStats(track);
    const inboundAudioStats: RTCInboundRtpStreamStats = extractStatEntry(audioStats, "inbound-rtp", "audio")!;
    return inboundAudioStats?.totalSamplesDuration || -1;
  };

  const audioTotalEnergyStart = await audioTotalEnergy(audioTrack);
  await sleep(checkInterval);
  const audioTotalEnergyEnd = await audioTotalEnergy(audioTrack);

  console.log("audio", audioTotalEnergyStart, audioTotalEnergyEnd);

  return audioTotalEnergyStart >= 0 && audioTotalEnergyEnd > audioTotalEnergyStart;
}

export async function inboundSimulcastStreamStats(room: Room) {
  const peerConnection = room.webrtc.connectionManager?.getConnection()!;

  const endpoints = Array.from(Object.values(room.webrtc.getRemoteEndpoints()));

  const stats = endpoints.map(async (peer: Endpoint<EndpointMetadata, TrackMetadata>) => {
    const videoTrack = room.getEndpointTrack(peer.id, "video");
    const track_stats = await peerConnection.getStats(videoTrack);
    const inbound_rtp = track_stats.get("inbound-rtp")

    return {
      height: inbound_rtp.frameHeight,
      width: inbound_rtp.frameWidth,
      framesPerSecond: inbound_rtp.framesPerSecond || 0,
      framesReceived: inbound_rtp.framesReceived,
      encoding: room.getPeerEncoding()
    }
  });

  return (await Promise.all(stats));
}

export async function outboundSimulcastStreamStats(room: Room) {
  const peerConnection = room.webrtc.connectionManager?.getConnection()!;
  const stats = await peerConnection.getStats();

  let data = { peerId: room.endpointId!, "l": {}, "m": {}, "h": {} }
  for (let [_key, report] of stats) {
    if (report.type == "outbound-rtp") {
      const rid: Encoding = report.rid
      data[rid] = {
        framesSent: report.framesSent,
        height: report.frameHeight,
        width: report.frameWidth,
        framesPerSecond: report.framesPerSecond ? report.framesPerSecond : 0
      } as EncodingStats
    }
  }

  return data
}

export async function remoteStreamsStats(room: Room) {
  const peerConnection = room.webrtc.connectionManager?.getConnection()!;
  const streams: MediaStream[] = Object.values(room.streams);

  console.log("streams stats - ", streams);

  const stats = streams.map(async (stream: MediaStream) => {
    const audioTracks = stream.getAudioTracks();
    const videoTracks = stream.getVideoTracks();

    const kind = audioTracks.length == 1 ? "audio" : "video";
    let playing = false;

    if (kind === "audio") {
      playing = await isAudioPlaying(peerConnection, audioTracks[0])
    }
    else {
      playing = await isVideoPlaying(peerConnection, videoTracks[0]);
    }

    console.log("audio", audioTracks, "video", videoTracks);

    return { streamId: stream.id, kind: kind, playing: playing }
  });

  const allStats = await Promise.all(stats);
  return allStats;
}
