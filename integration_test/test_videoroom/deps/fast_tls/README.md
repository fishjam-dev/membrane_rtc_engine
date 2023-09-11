# Fast TLS

[![CI](https://github.com/processone/fast_tls/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/processone/fast_tls/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/processone/fast_tls/badge.svg?branch=master&service=github)](https://coveralls.io/github/processone/fast_tls?branch=master)
[![Hex version](https://img.shields.io/hexpm/v/fast_tls.svg "Hex version")](https://hex.pm/packages/fast_tls)

Fast TLS is a native TLS / SSL driver for Erlang / Elixir. It is based
on [OpenSSL](https://www.openssl.org), a proven and efficient TLS
implementation.

It is designed for efficiency, speed and compliance.

## Installation

### Dependencies

Fast TLS depends on OpenSSL v1.0+. You need OpenSSL development
headers to build it. You can check your current OpenSSL version with `openssl version`.

### Generic build

You can trigger build with:

    ./configure && make

### OSX build example

On macOS the system copy of OpenSSL is usually too old, so you need to
install a newer OpenSSL version.

You can install OpenSSL with Homebrew:

    brew install openssl

You can then export environment variables to use OpenSSL as installed
by Homebrew, before issuing compilation commands:

    export LDFLAGS="-L/usr/local/opt/openssl/lib"
    export CFLAGS="-I/usr/local/opt/openssl/include/"
    export CPPFLAGS="-I/usr/local/opt/openssl/include/"

    ./configure && make

## Development

### Test

#### Unit test

You can run eunit test with the command:

    make test

