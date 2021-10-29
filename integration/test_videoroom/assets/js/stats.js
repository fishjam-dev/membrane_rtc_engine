
async function sleep(interval) {
  return new Promise((resolve, _reject) => {
    setTimeout(resolve, interval);
  });
}

// searches through RTCPStats entries iterator and tries
// to find an entry with a key complying with given prefix
function extractStatEntry(stats, prefix) {
  for (let [key, value] of stats) {
    if (key.startsWith(prefix)) {
      return value;
    }
  }

  return undefined;
}

// check interval is used to check if for given time interval any new packets/frames
// have been processed, it may happen that track was processing media before
// but somehow it stopped therefore we need to check deltas instead of
export async function remoteStreamsStats(peerConnection, checkInterval = 200) {
  const streams = peerConnection.getRemoteStreams();

  const audioSamplesReceived = async (track) => {
    if (!track) return -1;

    const audioStats = await peerConnection.getStats(track);
    const inboundAudioStats = extractStatEntry(audioStats, "RTCInboundRTPAudioStream");

    return inboundAudioStats ? inboundAudioStats.totalSamplesReceived : -1;
  };

  const videoFramedDecoded = async (track) => {
    if (!track) return -1;

    const videoStats = await peerConnection.getStats(track);
    const inboundVideoStats = extractStatEntry(videoStats, "RTCInboundRTPVideoStream");

    return inboundVideoStats ? inboundVideoStats.framesDecoded : -1;
  };

  const stats = streams.map(async (stream) => {
    const [audioTrack = undefined] = stream.getAudioTracks();
    const [videoTrack = undefined] = stream.getVideoTracks();

    const audioSamplesStart = await audioSamplesReceived(audioTrack);
    const videoFramesStart = await videoFramedDecoded(videoTrack);

    await sleep(checkInterval);

    const audioSamplesEnd = await audioSamplesReceived(audioTrack);
    const videoFramesEnd = await videoFramedDecoded(videoTrack);

    const isAudioPlaying =
      audioSamplesStart >= 0 && audioSamplesEnd >= 0 ? audioSamplesEnd > audioSamplesStart : false;
    const isVideoPlaying =
      videoFramesStart >= 0 && videoFramesEnd >= 0 ? videoFramesEnd > videoFramesStart : false;

    return { streamId: stream.id, isAudioPlaying, isVideoPlaying };
  });

  return Promise.all(stats);
}