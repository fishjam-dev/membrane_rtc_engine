# Simulcast

Simulcast is a technique where a client sends multiple encodings of the same video to the server and the server is responsbile for choosing and forwarding proper encoding to proper receiver (other client). The encoding selection is dynamic (i.e. SFU switches between encodings in time) and it is based on:

* receiver awailable bandwidth
* receiver preferences (e.g. explicit request to receive video in HD resolution instead of FHD)
* UI layaout (e.g. videos being displayed in smaller video tiles will be sent in lower resolution)

At the moment, Membrane supports only receiver preferences i.e. receiver can chose which encoding it is willing to receive. Additionaly, sender can turn off/on specific encoding. Membrane RTC Engine will detect changes and switch to another available encoding.Simulcast is a technique where client sends multiple encodings (different resolutions) of the same
video to a server and the server forwards proper encoding to each other client basing on 
client preferences, network bandwidth or UI layaout.

## Turning simulcast on/off

On the client side simulcast can be enabled while adding new track e.g.:

```ts
    // create MembraneWebRTC class instance
    // ...
    // add simulcasted track
    let trackId = webrtc.addTrack(track, stream, {}, true);
```

This will add a new track that will be sent in three versions:
* original (identified as `h`)
* original scaled down by 2 (identified as `m`)
* original scaled down by 4 (identified as `l`)

Those settings are not configurable at the moment.

> #### Minimal required resolution {: .warning}
>
> To make all encodings working, original track resolution has to be at least 1280x720.
> In other case, browser might not be able to scale resolution down.
> In case of browser, original track resolution can be specified in constraints
> passed to [`getUserMedia`](https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia) 
> or [`getDisplayMedia`](https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getDisplayMedia).

On the server side, simulcast can be disabled while adding new WebRTC Endpoint by setting its `simulcast?` 
option to `false`.
This will result in rejecting all incoming simulcast tracks i.e. client will not send them.

## Disabling and enabling specific track encoding

After adding simulcasted track, user can disable specific encoding.
For example, to disable the highest encoding:

```ts
    // create MembraneWebRTC class instance
    // ...
    // add simulcasted track
    let trackId = webrtc.addTrack(track, stream, {}, true);
    webrtc.disableTrackEncoding(trackId, "h");
```

Disabled encoding can be turned on again using `enableTrackEncoding` function.

Membrane RTC Engine tracks encoding activity. 
Therefore, when some encoding is turned off, RTC Engine will detect this and switch to 
the highest awailable encoding.
If turned off encoding returns, RTC Engine will switch back to it.

## Selecting encoding to receive

By default, RTC Engine will forward the highest available encoding.
However, this can be changed using `selectTrackEncoding` function.
For example, to receive the lowest resolution:

```ts
    webrtc.selectTrackEncoding(peerId, trackId, "l");
```

where `peerId` is id of peer this track belongs to.
