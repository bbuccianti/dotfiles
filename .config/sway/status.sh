#!/bin/sh

DATE=$(date +%e/%m\ %H:%M)
MAILS=$(/usr/bin/notmuch count tag:unread)
SONG=$(mpc status | head -1 | xargs -0 basename)

if test "$MAILS" != "0"
then
    notify-send --icon=mail-unread -t 30000 "$MAILS new emails"
fi

echo "$SONG | $MAILS | $DATE"
