#!/bin/sh

# Start the Erlang Port Mapper Daemon
epmd -daemon

if [ $# -gt 0 ]; then
  exec "$@"
else
  elixir --sname $(hostname) --cookie "$ERL_COOKIE" -S mix run --no-halt
fi
