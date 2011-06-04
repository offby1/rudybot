#!/bin/sh

# copy the big log from the running rudybot, since it's great test data.
rsync --partial --progress --recursive ec2:/usr/local/src/rudybot/big-log .
