#!/bin/bash
set -e

BASE=$(dirname $(readlink -f "$0"))

CSS="$BASE/modest.css"

SRC=${1:-"log_file_append.h"}
DST=${2:-$(basename $SRC .h).html}

PANDOC_FLAGS="--tab-stop=8 --toc -H $CSS --from markdown+smart --to html5 --standalone"

$BASE/code2md.awk < "$BASE/$SRC" | \
    pandoc $PANDOC_FLAGS \
           --metadata "title=${SRC}" \
           --metadata "date=$(date)" \
           --metadata "author=Paul Khuong" \
           -o $DST
