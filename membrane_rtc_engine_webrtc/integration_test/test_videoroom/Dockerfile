FROM elixir:1.14.3-otp-24-alpine

RUN \
  apk add --no-cache \
  git \
  nodejs \
  npm \
  gcc \
  musl-dev \
  pkgconfig \
  libsrtp-dev \
  openssl1.1-compat-dev \
  ffmpeg-dev

RUN mix local.hex --force && \
  mix local.rebar --force

ARG ENGINE_PATH=../membrane_rtc_engine
WORKDIR /membrane_rtc_engine
COPY ${ENGINE_PATH}/config config/
COPY ${ENGINE_PATH}/lib lib/
COPY ${ENGINE_PATH}/mix.exs ${ENGINE_PATH}/mix.lock ./
ENV RTC_ENGINE_PATH=/membrane_rtc_engine

ARG WEBRTC_PATH=../membrane_rtc_engine_webrtc
WORKDIR /membrane_rtc_engine_webrtc
COPY ${WEBRTC_PATH}/lib lib/
COPY ${WEBRTC_PATH}/mix.exs ${WEBRTC_PATH}/mix.lock ./

ARG APP_PATH=${WEBRTC_PATH}/integration_test/test_videoroom
ENV MIX_ENV=test

WORKDIR /app
COPY ${APP_PATH}/mix.exs ${APP_PATH}/mix.lock ./
RUN mix deps.get --only $MIX_ENV

COPY ${APP_PATH}/config/config.exs ${APP_PATH}/config/${MIX_ENV}.exs config/
RUN mix deps.compile

COPY ${APP_PATH}/lib lib/
RUN mix compile

COPY ${APP_PATH}/test test/

COPY ${APP_PATH}/config/runtime.exs config/

COPY ${APP_PATH}/assets assets/
RUN cd assets && npm install
RUN mix assets.deploy

# Generate self-signed cert for SSL
RUN mix phx.gen.cert

COPY ${APP_PATH}/docker-entrypoint.sh ./
RUN chmod +x docker-entrypoint.sh

ENTRYPOINT ["./docker-entrypoint.sh"]
