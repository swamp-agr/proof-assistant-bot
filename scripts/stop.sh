#!/bin/bash

set -e
set -x

# read PID from file
PID=$(cat ./proof-assistant-bot.pid)

# stop process
kill $PID

# drop PID from file
echo "" > ./proof-assistant-bot.pid
