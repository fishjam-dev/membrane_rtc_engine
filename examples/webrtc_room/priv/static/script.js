const pcConfig = { 'iceServers': [{ 'urls': 'stun:stun.l.google.com:19302' },] };
const videoPlayer = document.getElementById("videoPlayer");
const mediaConstraints = {video: true, audio: true}

const ws = new WebSocket(`ws://${window.location.host}/ws`);
ws.onopen = _ => start_connection(ws);
ws.onclose = event => console.log("WebSocket connection was terminated:", event);

const start_connection = async (ws) => {
  const pc = new RTCPeerConnection(pcConfig);
  pc.ontrack = event => {
    // TODO
  }
  pc.onicecandidate = ({candidate}) => {
    if (candidate === null) return;

    console.log("Sent ICE candidate:", candidate);
    sendMediaEvent("candidate", candidate);
  };

  const localStream = await navigator.mediaDevices.getUserMedia(mediaConstraints);
  videoPlayer.srcObject = localStream;
  for (const track of localStream.getTracks()) {
    pc.addTrack(track, localStream);
  }

  ws.onmessage = async event => {
    const {type, data: mediaEvent} = JSON.parse(event.data);
    if (type != "mediaEvent") {
      console.warn("Received unexpected WebSocket message:", event.data);
    }

    const {type: eventType, data: eventData} = JSON.parse(mediaEvent);
    switch (eventType) {
      case "offer":
        console.log("Received SDP offer:", eventData);
        // TODO
        break;
      case "answer":
        console.log("Received SDP answer:", eventData);
        await pc.setRemoteDescription(eventData)
        break;
      case "ice":
        console.log("Recieved ICE candidate:", eventData);
        await pc.addIceCandidate(eventData);
    }
  };

  const offer = await pc.createOffer();
  await pc.setLocalDescription(offer);
  console.log("Sent SDP offer:", offer);
  sendMediaEvent("offer", offer);
};

const sendMediaEvent = (type, data) => {
  const mediaEvent = JSON.stringify({type: type, data: data});
  ws.send(JSON.stringify({type: "mediaEvent", data: mediaEvent}));
}
