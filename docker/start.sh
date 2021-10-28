#!/bin/bash
set -eu

nohup redis-server &
/usr/local/bin/set-pin-code.sh 25
busybox crond -l 8 -f -L /dev/stderr

