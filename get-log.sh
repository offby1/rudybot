#!/bin/sh

set -e

rsync --progress xen:/usr/local/src/rudybot/big-log .
gzip < big-log > big-log.gz
