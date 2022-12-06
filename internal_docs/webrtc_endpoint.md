# WebRTC Endpoint

The WebRTC Endpoint is responsible for sending/receiving media tracks to/from a web borwser.


## Architecture

![Alt text](assets/webrtc_endpoint_architecture.svg)


## TrackSender

`TrackSender` is responsible for observing track activity and publishing track data to the engine.
In particular, it is responsible for sending `Membrane.RTC.Engine.Event.TrackVariantResumed` 
and `Membrane.RTC.Engine.Event.TrackVariantPaused` events, and replying to `Membrane.KeyframeRequestEvent`.

#### VariantTracker

A browser can pause sending some encoding when e.g. it doesn't have enough bandwidth.
This fact is not communicated to the server.
`VariantTracker` is responsible for tracking variant activity i.e. whether it is still active.

## TrackReceiver

`TrackReceiver` is responsible for receiving a track from the engine.
In particular, it is responsible for sending the `Membrane.RTC.Engine.Event.RequestTrackVariant` event, 
forwarding keyframe requests from the browser to the `TrackSender` and repairing RTP packets after
switching track variants.

## ConnectionAllocator
`ConnectionAllocator` is responsible for probing the connection and negotiating bandwidth with `TrackReceiver`.
The RTC Engine ships with two implementations of `ConnectionAllocator`:
* `Membrane.RTC.Engine.Endpoint.WebRTC.NoOpConnectionAllocator` - always grants bandwidth requested by `TrackReceiver`
* `Membrane.RTC.Engine.Endpoint.WebRTC.RTPConnectionAllocator` - probes the connection with RTP padding packets and grants bandwidth basing on GCC estimates

You can read more in `Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator` module docs.
