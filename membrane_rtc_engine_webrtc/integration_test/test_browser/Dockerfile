# Playwright doesn't play nicely with alpine.
# Moreover, the official Elixir container images seem to be missing several libraries necessary for
# the browsers to work. These libraries don't get installed when running `mix stampede.setup`...
FROM ubuntu:jammy

RUN \
  apt-get update && \
  apt-get -y install \
    locales \
    git \
    nodejs \
    iproute2 \
    erlang \
    unzip && \
  # Clear apt cache
  rm -rf /var/lib/apt/lists/*

ADD https://github.com/elixir-lang/elixir/releases/download/v1.14.4/elixir-otp-24.zip /tmp/ex.zip
RUN \
  unzip /tmp/ex.zip -d /elixir/ && \
  rm /tmp/ex.zip /elixir/bin/*.bat && \
  ln -s /elixir/bin/* /bin

# Set the locale
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN mix local.hex --force && \
  mix local.rebar --force

ENV MIX_ENV=test

WORKDIR /app

COPY mix.exs mix.lock ./
RUN mix deps.get --only $MIX_ENV && \
  mix deps.compile && \
  mix stampede.setup

COPY lib lib/
RUN mix compile

COPY test test/

COPY docker-entrypoint.sh ./
RUN chmod +x docker-entrypoint.sh

ENTRYPOINT ["./docker-entrypoint.sh"]
