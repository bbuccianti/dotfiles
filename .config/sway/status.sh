#!/bin/bash

DATE=$(date +%e/%m\ %H:%M)
ARTIST=$(echo -e "currentsong\nclose" | nc localhost 6600 | awk -F: '/Artist/ { print $2; }')
SONG=$(echo -e "currentsong\nclose" | nc localhost 6600 | awk -F: '/Title/ { print $2; }')

echo "$ARTIST | $SONG | $DATE"
