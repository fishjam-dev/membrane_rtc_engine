# Elixir bindings for [libsrtp]

[![Hex.pm](https://img.shields.io/hexpm/v/ex_libsrtp.svg)](https://hex.pm/packages/ex_libsrtp)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/ex_libsrtp/)
[![CircleCI](https://circleci.com/gh/membraneframework/ex_libsrtp.svg?style=svg)](https://circleci.com/gh/membraneframework/ex_libsrtp)

## Installation

Firstly, install [libsrtp]. Then, add `:ex_libsrtp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ex_libsrtp, "~> 0.6.0"}
  ]
end
```

## Usage

This library allows to convert RTP to SRTP and the other way round. The following snippet shows how to encrypt and decrypt a packet:

```elixir
iex> in_srtp = ExLibSRTP.new()
iex> ExLibSRTP.add_stream(in_srtp, %ExLibSRTP.Policy{ssrc: :any_inbound, key: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"})
:ok
iex> packet = <<128, 14, 15, 143, 98, 145, 127, 247, 233, 164, 145, 140, 1, 2, 3, 4>>
iex> {:ok, protected_packet} = ExLibSRTP.protect(in_srtp, packet)
{:ok,
 <<128, 14, 15, 143, 98, 145, 127, 247, 233, 164, 145, 140, 112, 112, 222, 241, 148, 205, 10, 185, 78, 20, 27, 103, 2, 207>>}
iex> out_srtp = ExLibSRTP.new()
iex> ExLibSRTP.add_stream(out_srtp, %ExLibSRTP.Policy{ssrc: :any_outbound, key: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"})
:ok
iex> {:ok, unprotected_packet} = ExLibSRTP.unprotect(out_srtp, protected_packet)
{:ok, <<128, 14, 15, 143, 98, 145, 127, 247, 233, 164, 145, 140, 1, 2, 3, 4>>}
iex> unprotected_packet == packet
true
```

## Copyright and License

Copyright 2020, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=ex_libsrtp)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=ex_libsrtp)

Licensed under the [Apache License, Version 2.0](LICENSE)

[libsrtp]: https://github.com/cisco/libsrtp
