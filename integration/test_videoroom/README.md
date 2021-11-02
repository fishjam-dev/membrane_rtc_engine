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

Tests are performed using a [wallaby](https://github.com/elixir-wallaby/wallaby) library
which allows to control browser actions using a selected web driver.

The default web driver for this set of tests is a `chromedriver`.

Tests are created in such a way that both `chrome` and `firefox` drivers are being used
in each test.


## Requirements
The `wallaby` library is setup to use `selenium-standalone-server` as its webdriver.
To make chrome and firefox work one must install both firefox and chrome drivers and make them
available for the selenium server.

For macos users one can install the server with:
```bash
brew install selenium-server
```

## Validating if clients are receiving media 
The main purpose of these tests is to check if the browser is
correctly sending and receiving media to/from other participants.

Chrome is quite straightforward to extract statistics from, we can check
if either audio or video is correctly. Firefox on the other hand does not expose
any useful statistics so the only thing we can check for is that a specific track is unmuted and that the
received packets counter gets incremented.

To check if that happens correctly, the information is extracted 
from underlying `PeerConnection` controlled by `MembraneWebRTC` js class
exposed by `membrane_rtc_engine`. All remotes streams get listed and each of them
is queried to return the information about its video/audio received statistics.

### Chrome
For audio stream `audioTotalEnergy` gets measured, for video it is `framesDecoded`.

### Firefox
For both audio and video `packetsReceived` gets measured.


To calculate if the media data is flowing a delta of those two measurements (for firefox and chrome) gets calculated on
an interval of `200 ms`. If the delta is `0` then it means that the stream is not receiving any 
media for either audio or video.

## Exposing the statistics to wallaby
By default there is no way to interact with a JS API from `wallby` level, so to
get around that limitation the gathered statistics are being put into a div elements
in a JSON encoded string that can be queried by `wallaby` and parsed to inspect the stats.

To gather current statistics the `wallaby` triggers a button in the web page that starts the process of gathering
the stats. Once the stats are ready, they get put inside the prepared div element and a `data-version` attribute of that div gets incremented.

The version attribute is necessary to differentiate between separate stats button clicks. The counter starts with `0` and for each completed stats
gathering it gets incremented by `1`. `wallaby` can use that information to wait for the div to get updated to this certain counter value
(it may seem like a hack but it is probably the only way to get around the limitation of the web driver and asynchronous nature of JS code).

So in all tests a version for each client's sessions is maintained and gets incremented once `wallaby` extracts the statistics.

## Testing scenarios
There are currently 3 major testing scenarios:
* clients gradually joining and leaving the room (up to 5 clients)
* clients joining all at once (5 clients)
* clients joining with: camera and mic, camera only, mic only, without any media sources (a single client for each of the cases, all in a single room at once)

## Running tests 
Before running tests, one must start a selenium standalone server so that the `wallaby` can make successful
requests to it.

For MacOS users it can be done either by running a brew service or manually with a following command (given that `selenium-server` is available from command line):
```bash
selenium-server standalone --port 4444
```

Then simply run:
```bash
mix test
```
