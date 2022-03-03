# TestVideoroom
Test application for running integration against `membrane_rtc_engine`.

## Testing environment
All tests are performed against a very simple, one view, Phoenix application.

The interface allows only for several actions:
* joining with camera and microphone
* joining with camera only 
* joining with microphone only 
* joining without any media sources 
* leaving the room 
* displaying statistics of all remote media streams

Tests are performed using a [playwright](https://github.com/geometerio/playwright-elixir) library
which allows to control browser actions using a selected web driver.


## Validating if clients are receiving media 
The main purpose of these tests is to check if the browser is
correctly sending and receiving media to/from other participants.

Chrome is quite straightforward to extract statistics from, we can check
if either audio or video is sent correctly. Firefox on the other hand does not expose
any useful statistics so the only thing we can check for is that a specific track is unmuted and that the
received packets counter gets incremented.

To check if that happens correctly, the information is extracted 
from underlying `PeerConnection` controlled by `MembraneWebRTC` js class
exposed by `membrane_rtc_engine`. All remote streams get listed and each of them
is queried to return the information about its video/audio received statistics.

### Chrome
For audio stream `audioTotalEnergy` gets measured, for video it is `framesDecoded`.

### Firefox
For both audio and video `packetsReceived` gets measured.


To calculate if the media data is flowing a delta of those two measurements (for firefox and chrome) gets calculated on
an interval of `200 ms`. If the delta is `0` then it means that the stream is not receiving any 
media for either audio or video.

## Exposing the statistics to playwright
By default there is no way to interact with a JS API from `playwright` level, so to
get around that limitation the gathered statistics are being put into a div elements
in a JSON encoded string that can be queried by `playwright` and parsed to inspect the stats.

To gather current statistics the `playwright` triggers a button in the web page that starts the process of gathering
the stats. Once the stats are ready, they get put inside the prepared div element and a `data-version` attribute of that div gets incremented.

The version attribute is necessary to differentiate between separate stats button clicks. The counter starts with `0` and for each completed stats
gathering it gets incremented by `1`. `playwright` can use that information to wait for the div to get updated to this certain counter value
(it may seem like a hack but it is probably the only way to get around the limitation of the web driver and asynchronous nature of JS code).

So in all tests a version for each client's sessions is maintained and gets incremented once `playwright` extracts the statistics.

## Testing scenarios
There are currently 3 major testing scenarios:
* clients gradually joining and leaving the room (up to 5 clients)
* clients joining all at once (5 clients)
* clients joining with: camera and mic, camera only, mic only, without any media sources (a single client for each of the cases, all in a single room at once)

## Running tests 

To start integration command simply run in `integration/test_videoroom`:
```bash
mix test 
```
To start integration command simply run in root of `membrane_rtc_engine`:
```bash
mix integration
```

To build and run integration tests from source run:
```bash
docker build  -t membrane_rtc_integration .
docker run -p 4001:4001 membrane_rtc_integration
```

To specify if you want run integration tests with or without integrated turns change `USE_INTEGRATED_TURN` flag in Dockerfile.


