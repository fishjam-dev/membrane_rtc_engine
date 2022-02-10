FROM elixir:1.12.2-alpine AS build

# install build dependencies
RUN \
    apk add --no-cache \
    build-base \
    npm \
    git \
    python3 \
    make \
    cmake \
    openssl-dev \ 
    libsrtp-dev \
    libnice-dev \
    ffmpeg-dev \
    opus-dev \
    clang-dev \
    fdk-aac-dev 

ARG VERSION
ENV VERSION=${VERSION}

# Create build workdir
WORKDIR /app

# install hex + rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# RUN npm install playwright --save-dev

# set build ENV
ENV MIX_ENV=prod

# install mix dependencies
COPY mix.exs mix.lock ./
RUN mix do deps.get, deps.compile

# build assets
COPY package.json package.json
COPY package-lock.json package-lock.json
COPY priv priv
COPY assets/package.json assets/package-lock.json ./assets/
RUN npm --prefix ./assets ci --progress=false --no-audit --loglevel=error

COPY assets assets
# RUN npm run --prefix ./assets deploy
# RUN mix phx.digest
RUN mix deps.get


# compile and build release
COPY lib lib
# RUN mix do compile, release
COPY integration integration

WORKDIR /app/integration/test_videoroom

RUN mix deps.update --all 
RUN mix deps.get
RUN npm install --prefix=assets esbuild
RUN npm install --prefix=assets ts-node
RUN npm install --prefix=assets typescript 
RUN npm i --prefix=assets 

# RUN mix deps.compile
# RUN mix phx.server
RUN mix do compile, release

# ENV HOME=/app

# EXPOSE 4000

# HEALTHCHECK CMD curl --fail http://localhost:4000 || exit 1  

# CMD ["bin/membrane_videoroom_demo", "start"]

# RUN mix test