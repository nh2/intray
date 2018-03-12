#!/bin/bash

set -e
set -x

./scripts/docker-build-intray-web-server.sh
docker run \
  --rm \
  --publish 8000:80 \
  --volume /tmp/shared-intray:/www/intray-data \
  intray-web-server:latest

