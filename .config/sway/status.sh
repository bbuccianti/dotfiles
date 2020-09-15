#!/bin/sh

DATE=$(date +%e/%m\ %H:%M)
COUNT=$(notmuch count tag:unread)
SONG=$(mpc status | head -1 | xargs -0 basename)

echo "$SONG | $COUNT | $DATE"
