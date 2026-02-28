#!/usr/bin/env bash
# Bootstrap a fresh Ubuntu EC2 instance for Fluxer: install Docker and Docker Compose.
# Run this on the server (e.g. after SSH), then clone the repo, add config, and run docker compose up -d.
# Usage: chmod +x aws-ec2-bootstrap.sh && ./aws-ec2-bootstrap.sh

set -e

echo "[fluxer-aws] Installing Docker and Docker Compose on Ubuntu..."

# Install Docker (official method for Ubuntu)
sudo apt-get update
sudo apt-get install -y ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc
echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu $(. /etc/os-release && echo "${VERSION_CODENAME:-$VERSION_CODENAME}") stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

# Allow current user to run docker without sudo (optional; ubuntu user on EC2)
sudo usermod -aG docker "$USER" 2>/dev/null || true

echo "[fluxer-aws] Docker version: $(docker --version)"
echo "[fluxer-aws] Docker Compose version: $(docker compose version 2>/dev/null || docker-compose --version 2>/dev/null || echo 'plugin not found')"
echo "[fluxer-aws] Bootstrap done. Next: clone Fluxer repo, add config/config.json, then run 'docker compose up -d' (see docs/AWS_DEPLOY.md)."
echo "[fluxer-aws] If you need to run docker without sudo, log out and back in (or run 'newgrp docker') so group membership applies."
