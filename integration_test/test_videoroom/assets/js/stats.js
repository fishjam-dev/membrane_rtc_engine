// check interval is used to check if for given time interval any new packets/frames
// have been processed, it may happen that track was processing media before
// but somehow it stopped therefore we need to check deltas instead of
const checkInterval = 200;

function detectBrowser() {
  if (typeof InstallTrigger !== undefined) return "firefox";
  if (window.chrome !== undefined) return "chrome";

  throw new Error("Unknown browser type");
}

async function sleep(interval) {
  return new Promise((resolve, _reject) => {
    setTimeout(resolve, interval);
  });
}

// searches through RTCPStats entries iterator and tries
// to find an entry with a key complying with given prefix
//
// works only for chrome...
function extractStatEntry(stats, prefix) {
  for (let [key, value] of stats) {
    if (key.startsWith(prefix)) {
      return value;
    }
  }

  return undefined;
}

async function isVideoPlayingChrome(peerConnection, videoTrack) {
  const videoFramedDecoded = async (track) => {
    if (!track) return -1;

    const videoStats = await peerConnection.getStats(track);
    const inboundVideoStats = extractStatEntry(videoStats, "RTCInboundRTPVideoStream");

    return inboundVideoStats ? inboundVideoStats.framesDecoded : -1;
  };

  const videoFramesStart = await videoFramedDecoded(videoTrack);
  await sleep(checkInterval);
  const videoFramesEnd = await videoFramedDecoded(videoTrack);

  return videoFramesStart >= 0 && videoFramesEnd >= 0 ? videoFramesEnd > videoFramesStart : false;
}

async function isAudioPlayingChrome(peerConnection, audioTrack) {
  const audioTotalEnergy = async (track) => {
    if (!track) return -1;

    const audioStats = await peerConnection.getStats(track);
    const inboundAudioStats = extractStatEntry(audioStats, "RTCInboundRTPAudioStream");

    return inboundAudioStats ? inboundAudioStats.totalAudioEnergy : -1;
  };

  const audioTotalEnergyStart = await videoFramedDecoded(videoTrack);
  await sleep(checkInterval);
  const audioTotalEnergyEnd = await audioTotalEnergy(audioTrack);

  return audioTotalEnergyStart >= 0 && audioTotalEnergyEnd >= 0
    ? audioTotalEnergyEnd > audioTotalEnergyStart
    : false;
}

async function isVideoPlayingFirefox(peerConnection, videoTrack) {
  const packetsReceived = (stats) => {
    const [, value] = Array.from(stats).find(([_key, value]) => value.mediaType === "video");

    return value.packetsReceived;
  };

  const packetsStart = packetsReceived(await peerConnection.getStats(videoTrack));
  await sleep(checkInterval);
  const packetsEnd = packetsReceived(await peerConnection.getStats(videoTrack));

  return packetsStart > 0 && packetsEnd > 0 && packetsEnd > packetsStart;
}

async function isAudioPlayingFirefox(peerConnection, audioTrack) {
  const packetsReceived = (stats) => {
    const [, value] = Array.from(stats).find(([_key, value]) => value.mediaType === "audio");

    return value.packetsReceived;
  };

  const packetsStart = packetsReceived(await peerConnection.getStats(audioTrack));
  await sleep(checkInterval);
  const packetsEnd = packetsReceived(await peerConnection.getStats(audioTrack));

  return packetsStart > 0 && packetsEnd > 0 && packetsEnd > packetsStart;
}

export async function remoteStreamsStats(peerConnection) {
  const streams = peerConnection.getRemoteStreams();

  const firefoxTrackActive = peerConnection
    .getReceivers()
    .map(({ track }) => track)
    .filter((track) => !track.muted)
    .map(({ id }) => id);

  const tmp = await peerConnection.getStats();
  for (let [key, value] of tmp) {
    console.log(key, value)
  }


  const stats = streams.map(async (stream) => {
    const [audioTrack = undefined] = stream.getAudioTracks();
    const [videoTrack = undefined] = stream.getVideoTracks();

    let data = { streamId: stream.id, isAudioPlaying: false, isVideoPlaying: false };

    switch (detectBrowser()) {
      case "chrome": {
        data.isAudioPlaying = await isAudioPlayingChrome(peerConnection, audioTrack);
        data.isVideoPlaying = await isVideoPlayingChrome(peerConnection, audioTrack);
        break;
      }
      case "firefox": {
        const isStreamActive =
          (audioTrack && firefoxTrackActive.includes(audioTrack.id)) ||
          (videoTrack && firefoxTrackActive.includes(videoTrack.id));
        if (!isStreamActive) {
          data.active = false;
        }

        data.isAudioPlaying =
          audioTrack !== undefined && (await isAudioPlayingFirefox(peerConnection, audioTrack));
        data.isVideoPlaying =
          videoTrack !== undefined && (await isVideoPlayingFirefox(peerConnection, videoTrack));
      }
    }

    return data;
  });

  return (await Promise.all(stats)).filter((data) => data.active === undefined);
}
