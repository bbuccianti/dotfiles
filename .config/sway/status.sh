#!/bin/sh

DATE=$(date +%e/%m\ %H:%M)
SONG=$(mpc status | head -1 | xargs -0 basename)

echo "$SONG | $DATE"
