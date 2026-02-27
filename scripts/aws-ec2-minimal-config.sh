#!/usr/bin/env bash
# Generate a minimal config/config.json for running Fluxer on EC2 with Docker Compose (valkey + fluxer_server).
# Usage: bash scripts/aws-ec2-minimal-config.sh [YOUR_PUBLIC_IP_OR_DOMAIN]
# Example: bash scripts/aws-ec2-minimal-config.sh 54.123.45.67
# If you omit the argument, base_domain will be "localhost" (you can edit config.json later).

set -e
PUBLIC_HOST="${1:-localhost}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CONFIG_DIR="$REPO_ROOT/config"
CONFIG_FILE="$CONFIG_DIR/config.json"

# Generate a 32-byte hex string (64 chars)
gen_hex() { openssl rand -hex 32 2>/dev/null || echo "CHANGE_ME_$(date +%s)_$(shuf -i 1000-9999 -n 1)"; }

MEDIA_SECRET=$(gen_hex)
ADMIN_BASE=$(gen_hex)
ADMIN_OAUTH=$(gen_hex)
MARKETING_BASE=$(gen_hex)
GATEWAY_SECRET=$(gen_hex)
NATS_TOKEN=$(gen_hex)
SUDO_SECRET=$(gen_hex)
CONN_SECRET=$(gen_hex)
MARKETING_SECRET=$(gen_hex)

mkdir -p "$CONFIG_DIR"
cat > "$CONFIG_FILE" << EOF
{
  "\$schema": "../packages/config/src/ConfigSchema.json",
  "env": "production",
  "domain": {
    "base_domain": "$PUBLIC_HOST",
    "public_scheme": "http",
    "public_port": 8080
  },
  "database": {
    "backend": "sqlite",
    "sqlite_path": "/usr/src/app/data/fluxer.db"
  },
  "internal": {
    "kv": "redis://valkey:6379/0",
    "kv_mode": "standalone"
  },
  "s3": {
    "access_key_id": "minimal",
    "secret_access_key": "minimal",
    "endpoint": "http://127.0.0.1:8080/s3"
  },
  "services": {
    "server": { "port": 8080, "host": "0.0.0.0" },
    "media_proxy": { "secret_key": "$MEDIA_SECRET" },
    "admin": {
      "secret_key_base": "$ADMIN_BASE",
      "oauth_client_secret": "$ADMIN_OAUTH"
    },
    "marketing": { "enabled": false, "secret_key_base": "$MARKETING_SECRET" },
    "gateway": {
      "port": 8082,
      "admin_reload_secret": "$GATEWAY_SECRET",
      "media_proxy_endpoint": "http://127.0.0.1:8080/media"
    },
    "nats": {
      "core_url": "nats://nats:4222",
      "jetstream_url": "nats://nats:4222",
      "auth_token": "$NATS_TOKEN"
    }
  },
  "auth": {
    "sudo_mode_secret": "$SUDO_SECRET",
    "connection_initiation_secret": "$CONN_SECRET",
    "vapid": {
      "public_key": "REPLACE_FOR_PUSH_NOTIFICATIONS",
      "private_key": "REPLACE_FOR_PUSH_NOTIFICATIONS"
    }
  }
}
EOF
echo "Created $CONFIG_FILE with base_domain=$PUBLIC_HOST (SQLite, Valkey, random secrets)."
echo "For a custom domain or HTTPS, edit domain.base_domain and domain.public_scheme/port in config.json."
