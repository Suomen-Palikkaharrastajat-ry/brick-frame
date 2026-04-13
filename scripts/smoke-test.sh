#!/usr/bin/env bash
set -e

curl -f --silent --output /dev/null "$SITE_URL"
curl -f --silent --output /dev/null "$SITE_URL/docs/"
curl -f --silent --output /dev/null "$SITE_URL/bricks-viewer.iife.js"

echo "Smoke test passed: $SITE_URL (+ web-components example)"
