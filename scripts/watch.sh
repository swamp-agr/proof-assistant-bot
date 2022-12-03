#!/bin/bash

set -e

# get bot PID from ps output.
PROOF_ASSISTANT_BOT_PID=$(ps aux | grep 'proof-assistant-bot' | grep -v grep | awk '{print $2}')

# test if process is running.
if ! [ -n "$PROOF_ASSISTANT_BOT_PID" ];
then
    . ./scripts/stop.sh
    sleep 5;
    . ./scripts/start.sh
fi
