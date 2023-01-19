#!/bin/bash

# Terminate on errors
set -e

deps="printf find git protoc protoc-gen-elixir"

for dep in $deps; do
  if ! [[ $(which $dep) ]]; then
    echo "Missing dependency: $dep"
    exit 1
  fi
done


printf "Synchronising submodules... "
git submodule sync --recursive >> /dev/null
git submodule update --recursive --init >> /dev/null
printf "DONE\n\n"

files=$(find protos/membrane_rtc_engine -name "*.proto")

printf "Compiling:\n"
count=1
total=${#files[@]}
for file in $files; do
  printf "[%i/%i] %s ... " $count $total $file
  protoc --elixir_out=./lib/ $file
  printf "DONE\n"
done
