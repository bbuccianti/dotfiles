#!/bin/sh

DATE=$(date +%e/%m\ %H:%M)
MAILS=$(/bin/notmuch count -inbox -unread)

if test "$MAILS" != "0"
then
    /bin/notify-send.sh --replace-file=/tmp/email "$MAILS new emails"
fi

echo "$MAILS | $DATE"
