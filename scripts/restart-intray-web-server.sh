#!/bin/bash
set -x

cd $HOME

killall intray-web-server

set -e

export PORT=8000
export API_PORT=8001

intray-web-server serve --persist-logins --admin admin &
