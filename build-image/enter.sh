#!/bin/sh

cd $(dirname $(readlink -f "$0"))

exec docker run  --platform linux/amd64 -v "$PWD/../":/home/user/Blog:z --network host -ti --rm localhost/blog
