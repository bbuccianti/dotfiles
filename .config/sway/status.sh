#!/bin/sh

DATE=$(date +%e/%m\ %H:%M)
MAILS=$(mu find flag:unread | wc -l)

if test "$MAILS" != "0"
then
   notify-send.sh --replace-file=/tmp/email_notification "$MAILS new emails"
fi

echo "$MAILS | $DATE"
