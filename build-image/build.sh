#!/bin/sh

set -e

cp ../Gemfile ../Gemfile.lock .

docker build -t blog \
  --build-arg USER_ID=$(id -u) \
  --build-arg GROUP_ID=$(id -g) .

docker run -v "$PWD/../":/home/user/Blog  -ti blog
