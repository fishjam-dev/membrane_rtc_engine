# ExDTLS

[![Hex.pm](https://img.shields.io/hexpm/v/ex_dtls.svg)](https://hex.pm/packages/ex_dtls)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/ex_dtls/)
[![CircleCI](https://circleci.com/gh/membraneframework/ex_dtls.svg?style=svg)](https://circleci.com/gh/membraneframework/ex_dtls)

DTLS and DTLS-SRTP handshake library for Elixir, based on [OpenSSL].

ElixirDTLS allows user to perform DTLS handshake (including DTLS-SRTP one) without requiring
any socket. Instead, it generates DTLS packets that user has to transport to the peer.
Thanks to this DTLS handshake can be performed on the third party socket e.g. one used to
establish connection via ICE protocol.

## Installation

The package can be installed by adding `ex_dtls` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ex_dtls, "~> 0.12.0"}
  ]
end
```

## Usage

`ExDTLS` can work both as a C node or as a NIF.
By default, C node implementation is used, however, user can change it by passing proper option while starting `ExDTLS`
or in `config.exs` by:

```elixir
config :ex_dtls, impl: :nif
```

Init `ExDTLS` on both peers with:

```elixir
# One peer should be a client and use client_mode: true, the other - false
# DTLS-SRTP is the most common use case for ExDTLS, we'll enable it
{:ok, dtls} = ExDTLS.start_link(client_mode: true, dtls_srtp: true)
```

On a peer running in a client mode start performing DTLS handshake

```elixir
{:ok, packets} = ExDTLS.do_handshake(dtls)
```

This will generate initial handshake packets. Now we have to pass them on the second peer.
You can use for that e.g. a UDP socket, but we will not cover this here.

After receiving initial DTLS packets on the second peer pass them to `ExDTLS`

```elixir
{:ok, packets} = ExDTLS.process(dtls, packets)
```

As a result, we will also get some new packets that have to be passed to the first peer.

After some back and forth DTLS handshake should be finished successfully.
Peer that finishes handshake first will return `{:finished, handshake_data, packets}`
message. These packets have to be sent to the second peer, so it can finish its handshake too and
return `{:finished, handshake_data}` message.

For more complete examples please refer to [membrane_ice_plugin] where we use `ex_dtls`
or to our integration tests.

## Copyright and License

Copyright 2020, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=ex_dtls)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=ex_dtls)

Licensed under the [Apache License, Version 2.0](LICENSE)

[OpenSSL]: https://www.openssl.org/
[membrane_ice_plugin]: https://github.com/membraneframework/membrane_ice_plugin
