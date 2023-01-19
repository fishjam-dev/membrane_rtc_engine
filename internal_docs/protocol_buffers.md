# Usage of Protocol Buffers in RTC Engine

Protocol Buffers are used to define messages exchanged between the WebRTC plugin and the client library.

## Sharing `.proto` file between RTC Engine and client libraries
In `membrane_rtc_engine`, a git submodule pointing to `@jellyfish-dev/protos` repository is defined.
WebRTC signalling-related `.proto` files should be placed in `membrane_rtc_engine/webrtc` directory.
The same approach is recommended for client libraries.
Because this approach seems to be the industry standard, and for consistency, in our official client libraries usage of git submodules should be enforced.

## Working with `.proto` files and ensuring backwards compatibility
After changing proto files, you will need to generate respective elixir files.
In order to do this you need the following dependencies:
- [protoc](https://grpc.io/docs/protoc-installation/)
- [protoc-gen-elixir](https://github.com/elixir-protobuf/protobuf#usage)

In order to generate the elixir files from `.proto`, run:
```bash
$ protoc --elixir_out=./lib/ protos/membrane_rtc_engine/**/*.proto
```

### Backward compatibility
Ensuring a reasonable level of backward compatibility is vital.
In general, we should aim to at maintain backwards compatibility for at least 6 months before enforcing the new variant, to give our users plenty of time to fully upgrade.
Of course, user will need to upgrade their clients to gain access to newest features.
Make sure to read the [Protobuf guide](https://developers.google.com/protocol-buffers/docs/proto#updating) on maintaining it and keep it in mind when making changes.

