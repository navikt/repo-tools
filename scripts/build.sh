#!/bin/bash
echo "Stack version: $(stack --numeric-version)"
echo "Node.js version: $(node --version)"
echo "npm version: $(npm --version)"
echo "Docker version: $(docker --version)"

(cd frontend && npm install && npm build && npm run webpack:production)

stack image container
