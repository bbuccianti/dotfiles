#!/bin/sh

DATE=$(date +%e/%m\ %H:%M)
MAILS=$(mu find flag:unread | wc -l)

if test "$MAILS" != "0"
then
    /bin/notify-send.sh --replace-file=/tmp/email "$MAILS new emails"
fi

echo "$MAILS | $DATE"
