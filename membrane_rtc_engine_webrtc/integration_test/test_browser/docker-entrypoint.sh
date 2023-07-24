#!/bin/sh

# If a command is passed to the script, execute it instead of the default one
if [ $# -gt 0 ]; then
  exec "$@"
else
  # Run the test, if successful then keep idling (no way to pass `--no-halt` to `mix test`)
  elixir --sname $(hostname) --cookie "$ERL_COOKIE" -S mix test && tail -f /dev/null
fi
