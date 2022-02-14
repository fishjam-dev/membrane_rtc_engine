FROM membrane/membrane:latest AS build

# Create build workdir
WORKDIR /app

# set build ENV
ENV MIX_ENV=test

# install mix dependencies
COPY mix.exs mix.lock ./

# build assets
COPY package.json package.json
COPY priv priv
COPY assets assets

# get deps and install libraries
COPY lib lib
COPY integration integration
RUN mix deps.get
RUN npm i esbuild --prefix=assets
RUN npm i typescript --prefix=assets
RUN npm ci --prefix=assets 
RUN mix deps.compile


WORKDIR /app/integration/test_videoroom

# Remove local assets
RUN rm -rf _build
RUN rm -rf deps
RUN rm -rf assets/node_modules
RUN rm -rf priv


# get deps and install libraries
RUN mix deps.update --all
RUN mix deps.get
RUN npm ci --prefix=assets
RUN npm i playwright@1.18.1 --prefix=assets
RUN npm i esbuild --prefix=assets

# Compile js app
RUN mix esbuild default --minify
RUN mix phx.digest


EXPOSE 4001


WORKDIR /app
CMD ["mix", "integration"]