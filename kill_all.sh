#!/bin/bash
echo "Killing all backend-related processes..."
pkill -u cantocanto -9 nats-server
pkill -u cantocanto -9 valkey-server
pkill -u cantocanto -9 meilisearch
pkill -u cantocanto -9 node
pkill -u cantocanto -9 caddy
echo "Cleanup done."
