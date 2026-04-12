#!/usr/bin/env bash
set -e

curl -f --silent --output /dev/null "$SITE_URL"
curl -f --silent --output /dev/null "$SITE_URL/examples/web-components/"
curl -f --silent --output /dev/null "$SITE_URL/examples/web-components/bricks-web-components.iife.js"

echo "Smoke test passed: $SITE_URL (+ web-components example)"
