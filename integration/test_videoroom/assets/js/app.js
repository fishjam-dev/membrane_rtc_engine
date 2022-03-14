import Room from "./room";
import { remoteStreamsStats } from "./stats";

const videos = document.querySelector("#videos");
const localVideo = document.querySelector("video#local-video");
const data = document.querySelector("div#data");

const startButtons = ["all", "mic-only", "camera-only", "none"].map((type) =>
  document.querySelector(`button#start-${type}`)
);

const [startAllButton, startMicOnlyButton, startCameraOnlyButton, startNoneButton] = startButtons;

const stopButton = document.querySelector("button#stop");
const statsButton = document.querySelector("button#stats");

startButtons.forEach((button) => (button.disabled = false));
stopButton.disabled = true;
statsButton.disabled = false;

let room;

async function start(media) {
  if (room) return;

  const preferences = {
    audio: ["all", "mic"].includes(media),
    video: ["all", "camera"].includes(media),
  };

  let localStream = undefined;
  if (preferences.audio || preferences.video) {
    localStream = await navigator.mediaDevices.getUserMedia(preferences);
    window.stream = localStream;
  }
  localVideo.srcObject = localStream;

  startButtons.forEach((button) => (button.disabled = true));
  stopButton.disabled = false;

  room = new Room(localStream);

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

async function refreshStats() {
  if (!room || !room.webrtc || !room.webrtc.connection) {
    data.innerHTML = `Room error. One of objects doesn't exists: Room ${!room}, WebRTC ${!room.webrtc}, PeerConnection ${!room
      .webrtc.connection}`;
    return;
  }
  // we are accessing room's private field, in the name of science of course...
  const stats = await remoteStreamsStats(room.webrtc.connection);

  window.pc = room.webrtc.connection;

  // put the statistics as text inside div
  data.innerHTML = JSON.stringify(stats);

  // update the current accessed version
  data.dataset.version = parseInt(data.dataset.version) + 1;
}

// setup all button callbacks
startAllButton.onclick = () => start("all");
startMicOnlyButton.onclick = () => start("mic");
startCameraOnlyButton.onclick = () => start("camera");
startNoneButton.onclick = () => start("none");
stopButton.onclick = stop;
statsButton.onclick = refreshStats;
