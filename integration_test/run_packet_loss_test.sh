#!/bin/bash

# test phase durations (in seconds)
LOSS_DURATION=60

SHARED_VOLUME_DIR="./tmp/shared"

APPLY_LOSS_TO="browser0"

# Terminate on errors
set -e

rm -rf $SHARED_VOLUME_DIR

docker compose build
echo "Running packet loss test"
docker compose up --exit-code-from=server server browser0 browser1 browser2 &
COMPOSE_JOB_ID=$!

cleanup() {
  docker compose down --rmi local --volumes
}

trap cleanup EXIT

echo "Sleeping until told to enable packet loss..."
while [ ! -f "${SHARED_VOLUME_DIR}/ENABLE_PACKET_LOSS" ]; do
  if ! jobs %% >/dev/null 2>&1; then
    echo "ERROR: docker compose job finished too early"
    exit 1
  fi

  sleep 1
done

echo "Applying packet loss to $APPLY_LOSS_TO for $LOSS_DURATION seconds"
pumba netem \
  --duration "${LOSS_DURATION}s" \
  loss \
  --percent 50 \
  $APPLY_LOSS_TO &

NETEM_JOB_ID=$!
trap "set +e; kill $NETEM_JOB_ID; cleanup" EXIT

wait $COMPOSE_JOB_ID
exit $?
