#!/bin/bash

set -e

# output errors and logs in the same stream
exec 2>&1

# prepare log directory
mkdir -p ./log

proof-assistant-bot +RTS -A64m -AL256m -qn2 -RTS >> log/proof-assistant-bot.log 2>&1 &

# write PID to file
echo $! > ./proof-assistant-bot.pid
