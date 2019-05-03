#!/bin/bash
set -e
echo "Stack version: $(stack --numeric-version)"
echo "Node.js version: $(node --version)"
echo "npm version: $(npm --version)"
echo "Docker version: $(docker --version)"
echo "Vault CLI version: $(vault --version)"

export VAULT_ADDR="https://vault.adeo.no"
SECRETS=$(vault read kv/preprod/fss/repo-tools/default -format=json) ||
    (echo "Please login to Vault, and try again: VAULT_ADDR=$VAULT_ADDR vault login -method=oidc" && exit 1)

export GITHUB_APP_PRIVATE_KEY=$(echo $SECRETS | jq ".data.GITHUB_APP_PRIVATE_KEY" -r)
export OIDC_CLIENT_ID=$(echo $SECRETS | jq ".data.OIDC_CLIENT_ID" -r)
export OIDC_CLIENT_SECRET=$(echo $SECRETS | jq ".data.OIDC_CLIENT_SECRET" -r)
export OIDC_DISCOVERY_URL=$(echo $SECRETS | jq ".data.OIDC_DISCOVERY_URL" -r)

(cd frontend && npm ci && npm run build && npm run webpack:production)

stack exec --docker-run-args="--net=bridge --publish=3000:3000 -v secrets.properties:/secrets.properties" -- yesod devel
