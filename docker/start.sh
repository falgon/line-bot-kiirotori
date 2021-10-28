#!/bin/bash
set -eu

readonly retry_max=10
readonly pincode_len=25
readonly log_level=8

nohup redis-server &
for((i=0;i<$retry_max;i++)); do
    if [[ "$(redis-cli ping 2>/dev/null)" = "PONG" ]]; then
        break
    fi
    echo "wait to boot redis server... (try number: `expr $((1 + i))`)"
    sleep 5
    if [[ "$i" = "`expr $((retry_max-1))`" ]]; then
        exit 1
    fi
done
/usr/local/bin/set-pin-code.sh $pincode_len
busybox crond -l $log_level -f -L /dev/stderr

