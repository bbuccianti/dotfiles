#!/bin/sh

DATE=$(date +%e/%m\ %H:%M)
SONG=$(mpc -f "[%artist% - %album% - %title%]" | head -n1)

echo "$SONG | $DATE"
