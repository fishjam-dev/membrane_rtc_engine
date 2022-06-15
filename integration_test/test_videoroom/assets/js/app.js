import Room from "./room";
import { remoteStreamsStats, inboundSimulcastStreamStats, outboundSimulcastStreamStats } from "./stats";

const videos = document.querySelector("#videos");
const localVideo = document.querySelector("video#local-video");
const data = document.querySelector("div#data");


const getButtonsWithPrefix = (types, prefix) => {
  return types.map((type) => document.querySelector(`button#${prefix}-${type}`))
}

const startButtons = getButtonsWithPrefix(["simulcast", "all", "mic-only", "camera-only", "none"], "start");

const simulcastButtons = getButtonsWithPrefix(["own-low", "own-medium", "own-high",
  "other-low", "other-medium", "other-high", "inbound-stats", "outbound-stats"], "simulcast")

const metadataButtons = getButtonsWithPrefix(["update-peer", "update-track", "peer", "track"], "metadata")

const [startSimulcastButton, startAllButton, startMicOnlyButton, startCameraOnlyButton, startNoneButton] = startButtons;
const [lowSimulcastButton, mediumSimulcastButton, highSimulcastButton,
  lowEncodingPeerButton, mediumEncodingPeerButton, highEncodingPeerButton, inboundSimulcastStatsButton, outboundSimulcastStatsButton] = simulcastButtons

const [updatePeerMetadataButton, updateTrackMetadataButton, peerMetadataButton, trackMetadataButton] = metadataButtons

const stopButton = document.querySelector("button#stop");
const statsButton = document.querySelector("button#stats");

startButtons.forEach((button) => (button.disabled = false));
metadataButtons.forEach((button) => (button.disabled = false));
simulcastButtons.forEach((button) => (button.disabled = true));
stopButton.disabled = true;
statsButton.disabled = false;

let room;

const simulcastPreferences = {
  width: { max: 1280, ideal: 1280, min: 1280 },
  height: { max: 720, ideal: 720, min: 720 },
  frameRate: { max: 30, ideal: 24 },
}

async function start(media, simulcast = false) {
  if (room) return;

  const useVideo = ["all", "camera"].includes(media);

  if (simulcast) {
    simulcastButtons.map(elem => elem.disabled = false)
  }

  const preferences = {
    audio: ["all", "mic"].includes(media),
    video: useVideo && simulcast ? simulcastPreferences : useVideo,
  };


  let localStream = undefined;
  if (preferences.audio || preferences.video) {
    localStream = await navigator.mediaDevices.getUserMedia(preferences);
    window.stream = localStream;
  }
  localVideo.srcObject = localStream;

  startButtons.forEach((button) => (button.disabled = true));
  stopButton.disabled = false;


  room = new Room(localStream, simulcast);

  await room.init();
  await room.join();
}

async function stop() {
  if (!room) return;

  room.leave();

  // remove children until we are left with the local video
  // tag which was the first one present
  while (videos.children.length > 1) {
    videos.removeChild(videos.lastChild);
  }

  room = undefined;

  startButtons.forEach((button) => (button.disabled = false));
  stopButton.disabled = true;
}

function putStats(stats) {
  window.pc = room.webrtc.connection;

  // put the statistics as text inside div
  data.innerHTML = JSON.stringify(stats);

  // update the current accessed version
  data.dataset.version = parseInt(data.dataset.version) + 1;
}

async function refreshStats() {
  if (!room || !room.webrtc || !room.webrtc.connection) {
    data.innerHTML = `Room error. One of objects doesn't exists: Room ${!room}, WebRTC ${!room.webrtc}, PeerConnection ${!room
      .webrtc.connection}`;
    return;
  }
  // we are accessing room's private field, in the name of science of course...
  const stats = await remoteStreamsStats(room.webrtc.connection);

  putStats(stats)
}

async function refreshSimulcastReceiverStats() {
  if (!room || !room.webrtc || !room.webrtc.connection) {
    data.innerHTML = `Room error. One of objects doesn't exists: Room ${!room}, WebRTC ${!room.webrtc}, PeerConnection ${!room
      .webrtc.connection}`;
    return;
  }
  // we are accessing room's private field, in the name of science of course...
  const stats = await inboundSimulcastStreamStats(room.webrtc.connection);
  stats.callbackEncoding = room.getPeerEncoding()

  putStats(stats)
}

async function refreshSimulcastSenderStats() {
  if (!room || !room.webrtc || !room.webrtc.connection) {
    data.innerHTML = `Room error. One of objects doesn't exists: Room ${!room}, WebRTC ${!room.webrtc}, PeerConnection ${!room
      .webrtc.connection}`;
    return;
  }
  // we are accessing room's private field, in the name of science of course...
  const stats = await outboundSimulcastStreamStats(room.webrtc.connection);

  putStats(stats)
}


const change = function (button, encoding) {
  const isEnabled = button.textContent.startsWith("Disable")
  let text = button.textContent
  if (isEnabled) {
    room.disableSimulcastEncoding(encoding)
    text = text.replace("Disable", "Enable")
  } else {
    room.enableSimulcastEncoding(encoding)
    text = text.replace("Enable", "Disable")
  }
  button.textContent = text
}

// setup all button callbacks
startSimulcastButton.onclick = () => start("all", true);
startAllButton.onclick = () => start("all");
startAllButton.onclick = () => start("all");
startMicOnlyButton.onclick = () => start("mic");
startCameraOnlyButton.onclick = () => start("camera");
startNoneButton.onclick = () => start("none");
stopButton.onclick = stop;
statsButton.onclick = refreshStats;
updatePeerMetadataButton.onclick = () => { room.updateMetadata() }
updateTrackMetadataButton.onclick = () => { room.updateTrackMetadata() }
peerMetadataButton.onclick = () => { putStats(room.peerMetadata) }
trackMetadataButton.onclick = () => { putStats(room.trackMetadata) }
lowSimulcastButton.onclick = () => { change(lowSimulcastButton, "l") }
mediumSimulcastButton.onclick = () => { change(mediumSimulcastButton, "m") }
highSimulcastButton.onclick = () => { change(highSimulcastButton, "h") }
lowEncodingPeerButton.onclick = () => { room.changePeerSimulcastEncoding("l") }
mediumEncodingPeerButton.onclick = () => { room.changePeerSimulcastEncoding("m") }
highEncodingPeerButton.onclick = () => { room.changePeerSimulcastEncoding("h") }
inboundSimulcastStatsButton.onclick = refreshSimulcastReceiverStats
outboundSimulcastStatsButton.onclick = refreshSimulcastSenderStats