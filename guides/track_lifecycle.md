# Track Lifecycle

There are two types of tracks - audio and video.

Video tracks might be in multiple qualities/resolutions.
We call them variants and indentify them using atoms - `:high`, `:medium`, `:low`.

In general:
* `:high` - main track resolution
* `:medium` - main track resolution scaled down by 2 
* `:low` - main track resolution scaled down by 4 

Audio tracks can have only one variant - `:high`.

At first, each track is created with `Membrane.RTC.Engine.Track.new/7`.
It is important that the engine supports only RTP tracks so each endpoint
that publishes some track has to publish it in the form of RTP packets.

> #### Unpacking RTP stream {: .tip}
> If you are writing an endpoint that consumes data from the engine
> you can get media out of RTP packets with the support of `Membrane.RTC.Engine.Track.get_depayloader/1`.

After creating a track it is published using `t:Membrane.RTC.Engine.publish_action_t/0`.
This causes the engine to notify other endpoints about new tracks and allows other endpoints
to subscribe for them.

Next, each track variant is marked as ready with `t:Membrane.RTC.Engine.track_ready_action_t/0`.
The action informs the engine that it can link bin's output pad to the engine itself.

At this point, the track is published and marked as ready.
Next section outlines how resuming, pausing and requesting track variants looks like.

## Managing track variants

### Resuming and pausing track variant

```ascii
Endpoint ---- TrackVariantResumed (:medium) ---> Engine
Endpoint ----        media (:medium)        ---> Engine
Endpoint ---- TrackVariantResumed (:low)    ---> Engine
Endpoint ----        media (:low)           ---> Engine
Endpoint ---- TrackVariantPaused (:medium)  ---> Engine
``` 

General rules:
* media can flow only when a track variant is in the resumed state
* when a track variant becomes inactive for some reason (e.g. browser stops sending it to the
server because of bandwidth limitation), such variant is marked as paused with
`Membrane.RTC.Engine.Event.TrackVariantPaused`.


### Requesting track variant

```ascii
Engine ---- TrackVariantResumed (:medium) ---> Endpoint
Engine <--- RequestTrackVariant (:medium) ---- Endpoint
Engine ---- TrackVariantResumed (:low)    ---> Endpoint
Engine ---- TrackVariantSwitched          ---> Endpoint
Engine ----        media                  ---> Endpoint
Engine ---- TrackVariantPaused  (:medium) ---> Endpoint
Engine <--- RequestTrackVariant (:low)    ---- Endpoint
``` 

General rules:
* after requesting a track variant but before receiving the
very first media packets, there is always the 
`Membrane.RTC.Engine.Event.TrackVariantSwitched` event.
* if requested track variant becomes inactive before being delivered,
it has to be re-requested once it becomes active again
* sending the `Membrane.RTC.Engine.Event.RequestTrackVariant` event 
causes engine to send the `Membrane.KeyframeRequestEvent` event to the track origin.
Once engine receives a keyframe from the publisher it will start forwarding 
requested track.