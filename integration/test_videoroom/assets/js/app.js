// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html"
import Room from "./room";
import { remoteStreamsStats } from "./stats";
// Establish Phoenix Socket and LiveView configuration.

const videos = document.querySelector("#videos");
const localVideo = document.querySelector("video#local-video");
const data = document.querySelector("div#data");

const startButton = document.querySelector("button#start");
const stopButton = document.querySelector("button#stop");
const statsButton = document.querySelector("button#stats");

startButton.disabled = false;
stopButton.disabled = true;
statsButton.disabled = false;


let room;

async function start() {
  if (room) return;

  const localStream = await navigator.mediaDevices.getUserMedia({
    audio: true,
    video: true,
  });
  
  localVideo.srcObject = localStream;

  startButton.disabled = true;
  stopButton.disabled = false;

  room = new Room(localStream);
  window.room = room;
  
  await room.init();
  await room.join();
}

async function stop() {
  if (!room) return;
  
  room.leave()
  
  while (videos.children.length > 1) {
    videos.removeChild(videos.lastChild);
  } 
  
  room = undefined;

  startButton.disabled = false;
  stopButton.disabled = true;
}

async function refreshStats() {
  if (!room || !room.webrtc || !room.webrtc.connection) return;
  window.pc = room.webrtc.connection;
  
  // we are accessing room's private field...
  const stats = await remoteStreamsStats(room.webrtc.connection);
  
  data.innerHTML = JSON.stringify(stats);
  data.dataset.version = parseInt(data.dataset.version) + 1;
}

startButton.onclick = start;
stopButton.onclick = stop;
statsButton.onclick = refreshStats;
