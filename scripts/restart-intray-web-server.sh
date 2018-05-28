#!/bin/bash
set -x

cd $HOME

killall intray-web-server

set -e

export PORT=8002
export API_PORT=8003

intray-web-server serve --persist-logins --admin admin &
