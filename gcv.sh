#!/usr/bin/env bash

cd $(dirname $0)

if [ -p /dev/stdin ]; then
  cat - | stack run | bash | python3
fi
