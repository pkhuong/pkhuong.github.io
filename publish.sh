#!/bin/sh

cd ..
rsync -a --delete Blog/public/* pkhuong.github.io/
