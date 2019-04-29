#!/bin/bash
echo "Stack version: $(stack --numeric-version)"
echo "Node.js version: $(node --version)"
echo "npm version: $(npm --version)"
echo "Docker version: $(docker --version)"

(cd frontend && npm install && npm build && npm run webpack:production)

stack exec --docker-run-args="--net=bridge --publish=3000:3000 -v secrets.properties:/secrets.properties" -- yesod devel
