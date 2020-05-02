#!/bin/sh

DATE=$(date +%e/%m\ %H:%M)
MAILS=$(mu find flag:unread | wc -l)

if test "$MAILS" != "0"
then
    echo 2525 > /tmp/email
    /bin/notify-send.sh --replace-file=/tmp/email "$MAILS new emails"
fi

echo "$MAILS | $DATE"
