#!/bin/sh

# If a command is passed to the script, execute it instead of the default one
if [ $# -gt 0 ]; then
  exec "$@"
else
  elixir --sname $(hostname) --cookie "$ERL_COOKIE" -S mix test.containerised --only packet_loss_test
fi
