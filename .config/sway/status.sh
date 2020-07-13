#!/bin/sh

DATE=$(date +%e/%m\ %H:%M)
MAILS=$(/bin/notmuch count tag:unread)

if test "$MAILS" != "0"
then
    notify-send --icon=mail-unread -t 30000 "$MAILS new emails"
fi

echo "$MAILS | $DATE"
