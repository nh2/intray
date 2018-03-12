#!/bin/bash

set -e
set -x

stack build :intray-web-server
binpathabs=$(stack path --local-install-root)/bin/intray-web-server
mkdir -p deploy/dist
binpathrel="deploy/dist/intray-web-server"
cp "${binpathabs}" "${binpathrel}"
docker build -t intray-web-server deploy
