#!/bin/bash
# Absolute paths to Nix-managed binaries
export PATH=/nix/store/f6vvh77q97s6rkja9g37x8nmyhvpsj42-devenv-profile/bin:$PATH
ROOT=$(pwd)
export FLUXER_CONFIG=$ROOT/config.json

echo "Stopping existing services..."
killall nats-server valkey-server meilisearch node caddy 2>/dev/null

echo "Starting NATS Core..."
nohup nats-server -p 4222 -a 127.0.0.1 > nats_core.log 2>&1 &

echo "Starting NATS JetStream..."
nohup nats-server -p 4223 -js -sd $ROOT/dev/data/nats_jetstream -a 127.0.0.1 > nats_js.log 2>&1 &

echo "Starting Valkey..."
nohup valkey-server --bind 127.0.0.1 --port 6379 > valkey.log 2>&1 &

echo "Starting Meilisearch..."
nohup meilisearch --http-addr 127.0.0.1:7700 --env development --master-key 3e794a04937b0d64a249ebcba9141afc1db50b940236362faccdb0cc27f49223 --db-path $ROOT/dev/data/meilisearch > meili.log 2>&1 &

echo "Waiting for dependencies..."
sleep 5

echo "Starting Fluxer Server..."
export PATH=$ROOT/node_modules/.bin:$PATH
nohup pnpm --filter fluxer_server dev > server.log 2>&1 &

echo "Starting Caddy..."
nohup caddy run --config dev/Caddyfile.dev --adapter caddyfile > caddy.log 2>&1 &

echo "Startup script finished."
