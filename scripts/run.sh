#!/bin/sh

export GITHUB_APP_PRIVATE_KEY=$(cat /opt/app/secrets/GITHUB_APP_PRIVATE_KEY)
export OIDC_DISCOVERY_URL=$(cat /opt/app/secrets/OIDC_DISCOVERY_URL)
export OIDC_CLIENT_ID=$(cat /opt/app/secrets/OIDC_CLIENT_ID)
export OIDC_CLIENT_SECRET=$(cat /opt/app/secrets/OIDC_CLIENT_SECRET)

/opt/app/bin/repo-tools

sleep 60
