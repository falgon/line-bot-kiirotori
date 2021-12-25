#!/bin/bash

set -eu

redis-cli SET PINCODE $(cat /dev/urandom | base64 | fold -w $1 | head -n1)
