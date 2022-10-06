# Track Lifecycle

There are two types of tracks - audio and video.

Video tracks might be in multiple qualities/resolutions.
We call them variants and indentify them using atoms - `:high`, `:medium`, `:low`.

In general:
* high - main track resolution
* medium - main track resolution scaled down by 2 
* low - main track resolution scaled down by 4 

Audio tracks can have only one variant - `:high`.

The whole flow starts with instantiating a new track with `Membrane.RTC.Engine.Track.new/7`.
It is important that engine supports only RTP tracks.
To unpack media out of RTP packets you can use `Membrane.RTC.Engine.Track.get_depayloader/1`.

After creating a track it is published  using `t:Membrane.RTC.Engine.publish_action_t/0`.
This will cause engine notyfing other endpoints about new tracks and allow other endpoints
to subscribe for them.

After publishing the track, an endpoint has to mark each variant as ready with
`t:Membrane.RTC.Engine.track_ready_action_t/0`.
The action will inform engine that it can link our bin's output pad to the engine itself.

Next, before sending any media data for given track variant we have to resume it 
at first with `Membrane.RTC.Engine.Event.TrackVariantResumed`.

When track variant becomes inactive for some reason (e.g. browser stops sending it to the
server because of bandwidth limitation), such track should be marked as paused with
`Membrane.RTC.Engine.Event.TrackVariantPaused`.

The whole flow is summarized below

1. Publish track with `t:Membrane.RTC.Engine.publish_action_t/0`
2. Mark variant as ready with `t:Membrane.RTC.Engine.track_ready_action_t/0`
3. Mark variant as resumed with `Membrane.RTC.Engine.Event.TrackVariantResumed`
4. Start sending media data
5. If some track variant is no longer available pause it with `Membrane.RTC.Engine.Event.TrackVariantPaused`.
