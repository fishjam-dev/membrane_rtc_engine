# Integration tests

Directory containing test scenarios for running integration against `membrane_rtc_engine` with the
use of containerised headless browsers and containerised `test_videoroom` app.

To run the container-less suite of integration tests, refer to [the videoroom app](test_videoroom/README.md).

## Test scenarios

### Packet loss

Simple test using four containers: `test_videoroom` and three `test_browser`s. The browsers connect
to videoroom, start sending and receiving media, then we apply packet loss on one of the browser
containers and verify that other browsers receive less frames only from this particular one.

To run, execute `./test_packet_loss.sh`
