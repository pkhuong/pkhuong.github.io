#!/bin/sh

set -e

cd $(dirname $(readlink -f "$0"))

cp ../Gemfile .

cp ../Gemfile.lock . || echo "No lock file."

docker build -t blog \
  --build-arg USER_ID=$(id -u) \
  --build-arg GROUP_ID=$(id -g) .

exec ./enter.sh
