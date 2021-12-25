#!/bin/bash

set -euo pipefail

docker exec -it redis-line-bot-kiirotori redis-cli --raw GET PINCODE

