#!/usr/bin/env bash
set -e
curl -f --silent --output /dev/null "$SITE_URL"
echo "Smoke test passed: $SITE_URL"
