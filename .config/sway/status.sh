#!/bin/sh

DATE=$(date +%e/%m\ %H:%M)
MAILS=$(mu find flag:unread | wc -l)

echo "$MAILS | $DATE"
