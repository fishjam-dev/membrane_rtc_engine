# Membrane WebRTC video room demo

This project demonstrates an example usage of the Membrane RTC Engine API.

## Run manually

Below is the instruction for the installation of required dependencies and how to run this demo on various operating systems:

<details>
<summary>
<b>macOS Intel</b>
</summary>

### Prerequisites

Make sure you have `node.js`, `openssl`, `FFmpeg`, and `srtp` installed on your computer.

```shell
brew install srtp libnice clang-format ffmpeg opus openssl pkg-config
```

Then add the following environment variables to your shell (`~/.zshrc`):

```shell
export LDFLAGS="-L/usr/local/opt/openssl@1.1lib"
export CFLAGS="-I/usr/local/opt/openssl@1.1/include/"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include/"
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig"
```

and restart your terminal.

Furthermore, make sure you have Elixir installed on your machine. For installation details, see: https://elixir-lang.org/install.html

### Running the demo

To run the demo, clone the `membrane_demo` repository and checkout to the demo directory:

```shell
git clone https://github.com/membraneframework/membrane_demo
cd membrane_demo/webrtc_videoroom
```

Then you need to download the dependencies of the mix project:

```shell
mix deps.get
npm ci --prefix=assets
```

You may be asked to install `Hex` and then `rebar3`.

To run the demo, type:

```shell
EXTERNAL_IP=<IPv4 address> mix phx.server
```

where:

- `EXTERNAL_IP` - your local IPv4 address of the computer this is running on. It is required unless you only connect via localhost (not to be confused with loopback).

To make the server available from your local network, you can set it to a private address, like 192.168._._. The address can be found with the use of the `ifconfig` command:

```shell
ifconfig
...
en0: flags=8863<UP,BROADCAST,SMART,RUNNING,SIMPLEX,MULTICAST> mtu 1500
 options=400<CHANNEL_IO>
 ether 88:66:5a:49:ac:e0
 inet6 fe80::426:8833:1408:cd1a%en0 prefixlen 64 secured scopeid 0x6
 inet 192.168.1.196 netmask 0xffffff00 broadcast 192.168.1.255
 nd6 options=201<PERFORMNUD,DAD>
 media: autoselect
 status: active
```

(The address we are seeking is the address following the inet field - in that particular case, 192.168.1.196)

Then go to <http://localhost:4000/>.

_You might be asked to grant access to your camera, as some operating systems require that._

_In case of the absence of a physical camera, it is necessary to use a virtual camera (e.g. OBS, [see how to set up the virtual camera in OBS](https://obsproject.com/kb/virtual-camera-guide))_

</details>

<details>
<summary>
<b>macOS M1/M2</b>
</summary>

### Prerequisites

Make sure you have `node.js`, `openssl`, `FFmpeg`, and `srtp` installed on your computer.

```shell
brew install srtp libnice clang-format ffmpeg opus openssl pkg-config
```

Then add the following environment variables to your shell (`~/.zshrc`):

```shell
export C_INCLUDE_PATH="/opt/homebrew/Cellar/libnice/0.1.18/include:/opt/homebrew/Cellar/opus/1.4/include:/opt/homebrew/Cellar/openssl@1.1/1.1.1l_1/include"
export PKG_CONFIG_PATH="/opt/homebrew/Cellar/openssl@1.1/1.1.1u/lib/pkgconfig"
export LDFLAGS="-L/opt/homebrew/Cellar/openssl@1.1/1.1.1u/lib"
export CFLAGS="-I/opt/homebrew/Cellar/openssl@1.1/1.1.1u/include"
export CPPFLAGS="-I/opt/homebrew/Cellar/openssl@1.1/1.1.1u/include"
```

and restart your terminal.

Furthermore, make sure you have Elixir installed on your machine. For installation details, see: https://elixir-lang.org/install.html

### Running the demo

To run the demo, clone the `membrane_demo` repository and checkout to the demo directory:

```shell
git clone https://github.com/membraneframework/membrane_demo
cd membrane_demo/webrtc_videoroom
```

Then you need to download the dependencies of the mix project:

```shell
mix deps.get
npm ci --prefix=assets
```

You may be asked to install `Hex` and then `rebar3`.

To run the demo, type:

```shell
EXTERNAL_IP=<IPv4 address> mix phx.server
```

where:

- `EXTERNAL_IP` - your local IPv4 address of the computer this is running on. It is required unless you only connect via localhost (not to be confused with loopback).

To make the server available from your local network, you can set it to a private address, like 192.168._._. The address can be found with the use of the `ifconfig` command:

```shell
ifconfig
...
en0: flags=8863<UP,BROADCAST,SMART,RUNNING,SIMPLEX,MULTICAST> mtu 1500
 options=400<CHANNEL_IO>
 ether 88:66:5a:49:ac:e0
 inet6 fe80::426:8833:1408:cd1a%en0 prefixlen 64 secured scopeid 0x6
 inet 192.168.1.196 netmask 0xffffff00 broadcast 192.168.1.255
 nd6 options=201<PERFORMNUD,DAD>
 media: autoselect
 status: active
```

(The address we are seeking is the address following the inet field - in that particular case, 192.168.1.196)

Then go to <http://localhost:4000/>.

_You might be asked to grant access to your camera, as some operating systems require that._

_In case of the absence of a physical camera, it is necessary to use a virtual camera (e.g. OBS, [see how to set up the virtual camera in OBS](https://obsproject.com/kb/virtual-camera-guide))_

</details>

<details>
<summary>
<b>Ubuntu</b>
</summary>

### Prerequisites

Make sure you have `node.js`, `openssl`, `FFmpeg`, and `srtp` installed on your computer.

```shell
sudo apt-get install libsrtp2-dev libnice-dev libavcodec-dev libavformat-dev libavutil-dev libopus-dev libssl-dev
```

Furthermore, make sure you have Elixir installed on your machine. For installation details, see: https://elixir-lang.org/install.html

On Ubuntu, we recommend installation through `asdf`, see: https://asdf-vm.com/guide/getting-started.html

### Running the demo

To run the demo, clone the `membrane_demo` repository and checkout to the demo directory:

```shell
git clone https://github.com/membraneframework/membrane_demo
cd membrane_demo/webrtc_videoroom
```

Then you need to download the dependencies of the mix project:

```shell
mix deps.get
npm ci --prefix=assets
```

You may be asked to install `Hex` and then `rebar3`.

> In case of installation issues with Hex on Ubuntu, try updating the system packages first by entering the command:
>
> ```shell
> sudo apt-get update
> ```

To run the demo, type:

```shell
EXTERNAL_IP=<IPv4 address> mix phx.server
```

where:

- `EXTERNAL_IP` - your local IPv4 address of the computer this is running on. It is required unless you only connect via localhost (not to be confused with loopback).

To make the server available from your local network, you can set it to a private address, like 192.168._._. The address can be found with the use of the `ifconfig` command:

```shell
ifconfig
...
en0: flags=8863<UP,BROADCAST,SMART,RUNNING,SIMPLEX,MULTICAST> mtu 1500
 options=400<CHANNEL_IO>
 ether 88:66:5a:49:ac:e0
 inet6 fe80::426:8833:1408:cd1a%en0 prefixlen 64 secured scopeid 0x6
 inet 192.168.1.196 netmask 0xffffff00 broadcast 192.168.1.255
 nd6 options=201<PERFORMNUD,DAD>
 media: autoselect
 status: active
```

(The address we are seeking is the address following the inet field - in that particular case, 192.168.1.196)

Then go to <http://localhost:4000/>.

_You might be asked to grant access to your camera, as some operating systems require that._

_In case of the absence of a physical camera, it is necessary to use a virtual camera (e.g. OBS, [see how to set up the virtual camera in OBS](https://obsproject.com/kb/virtual-camera-guide))_

</details>

## Copyright and License

Copyright 2020, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane)

Licensed under the [Apache License, Version 2.0](LICENSE)
