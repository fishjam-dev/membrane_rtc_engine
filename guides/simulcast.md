# Simulcast

Simulcast is a technique where a client sends multiple encodings of the same video to the server and the server is responsible for choosing and forwarding proper encoding to proper receiver (other client). The encoding selection is dynamic (i.e. SFU switches between encodings in time) and it is based on:

* receiver available bandwidth
* receiver preferences (e.g. explicit request to receive video in HD resolution instead of FHD)
* UI layout (e.g. videos being displayed in smaller video tiles will be sent in a lower resolution)

At the moment, Membrane supports only receiver preferences i.e. receiver can chose which encoding it is willing to receive. Additionally, sender can turn off/on specific encoding. Membrane RTC Engine will detect changes and switch to another available encoding.

## Turning simulcast on/off

### Server side 

On the server side, simulcast can be configured while adding new WebRTC Endpoint by setting its `simulcast_config` option.
Remember to ensure `Rid`, `Mid` and `TWCC` extensions are used.

For example

```elixir
alias Membrane.RTC.Engine.Endpoint.WebRTC
alias Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastConfig
alias Membrane.WebRTC.Extension.{Mid, Rid, TWCC}
# ...
%WebRTC{
  rtc_engine: rtc_engine,
  # ...
  webrtc_extensions: [Rid, Mid, TWCC],
  simulcast_config: %SimulcastConfig{
    enabled: true,
    initial_target_variant: fn _track_ -> :medium end
  }
}
```

Here we turn simulcast on and choose `:medium` variant as a target for each track
that is going to be forwarded to the browser.
Target variant means that it will be chosen whenever it is active.
If for some reason `:medium` variant is inactive we will temporarily forward some other variant.

On the other hand, setting `enabled` to `false` will result in rejecting all incoming simulcast tracks i.e. client will not send them to the server.

### Client side

On the client side simulcast can be enabled while adding a new track and might differ depending
on the client type e.g. for JS see https://docs.membrane.stream/membrane-webrtc-js/classes/membranewebrtc.html#addtrack
