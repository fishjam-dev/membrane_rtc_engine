# Voice Activity Detection
Voice Activity Detection (VAD) is a feature available in Membrane RTC Engine that allows you to receive notifications about voice activity.
You may also know it as speech detection.

After enabling this feature, you're going to start receiving `vadNotification` media events in your client application.
These media events contain a `trackId` and `vadStatus` - either speech or silence.

Depending on the SDK, a callback or other means of publishing this information may be available.
Please refer to the documentation of your SDK.

## Enabling VAD
To enable VAD you need to
1. Enable VAD RTP header extension
2. Enable VAD WebRTC extension

In practice, ensure that:
1. `Membrane.WebRTC.Extension.VAD` is present in the `webrtc_extensions` key of your WebRTC Endpoint configuration
2. The following is part of the `extensions` key in your WebRTC Endpoint configuration
```elixir
%{
  opus: Membrane.RTP.VAD
}
```
