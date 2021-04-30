#!/bin/sh

cd $(dirname $(readlink -f "$0"))

exec docker run -v "$PWD/../":/home/user/Blog --network host -ti blog
