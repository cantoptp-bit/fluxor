{ pkgs, config, lib, ... }:
{
	imports = lib.optional (builtins.pathExists ./devenv.local.nix) ./devenv.local.nix;

	env = {
		FLUXER_CONFIG = "${config.git.root}/config/config.json";
		FLUXER_DATABASE = "cassandra";
		PC_DISABLE_TUI = "1";
	};

	dotenv.enable = false;
	cachix.enable = false;

	process.manager.implementation = "process-compose";

	packages = with pkgs; [
		nodejs_24
		pnpm
		erlang_28
		rebar3
		valkey
		meilisearch
		nats-server
		ffmpeg
		exiftool
		caddy
		livekit
		mailpit
		go_1_24
		(rust-bin.stable.latest.default.override {
			targets = [ "wasm32-unknown-unknown" ];
		})
		jq
		gettext
		lsof
		iproute2
		python3
		pkg-config
		gcc
		gnumake
		sqlite
		openssl
		curl
		uv
	];

	tasks."fluxer:bootstrap" = {
		exec = "${config.git.root}/scripts/dev_bootstrap.sh";
		before = [
			"devenv:processes:meilisearch"
			"devenv:processes:fluxer_server"
			"devenv:processes:fluxer_app"
			"devenv:processes:marketing_dev"
			"devenv:processes:css_watch"
			"devenv:processes:fluxer_gateway"
			"devenv:processes:livekit"
			"devenv:processes:mailpit"
			"devenv:processes:valkey"
			"devenv:processes:caddy"
			"devenv:processes:nats_core"
			"devenv:processes:nats_jetstream"
		];
	};

	tasks."cassandra:mig:create" = {
		exec = ''
			name="$(echo "$DEVENV_TASK_INPUT" | jq -r '.name // empty')"
			if [ -z "$name" ]; then
				echo "Missing --input name" >&2
				exit 1
			fi
			cd "${config.git.root}/fluxer_api"
			pnpm tsx scripts/CassandraMigrate.tsx create "$name"
		'';
	};

	tasks."cassandra:mig:check" = {
		exec = ''
			cd "${config.git.root}/fluxer_api"
			pnpm tsx scripts/CassandraMigrate.tsx check
		'';
	};

	tasks."cassandra:mig:status" = {
		exec = ''
			host="$(echo "$DEVENV_TASK_INPUT" | jq -r '.host // "localhost"')"
			user="$(echo "$DEVENV_TASK_INPUT" | jq -r '.user // "cassandra"')"
			pass="$(echo "$DEVENV_TASK_INPUT" | jq -r '.pass // "cassandra"')"
			cd "${config.git.root}/fluxer_api"
			pnpm tsx scripts/CassandraMigrate.tsx --host "$host" --username "$user" --password "$pass" status
		'';
	};

	tasks."cassandra:mig:up" = {
		exec = ''
			host="$(echo "$DEVENV_TASK_INPUT" | jq -r '.host // "localhost"')"
			user="$(echo "$DEVENV_TASK_INPUT" | jq -r '.user // "cassandra"')"
			pass="$(echo "$DEVENV_TASK_INPUT" | jq -r '.pass // "cassandra"')"
			cd "${config.git.root}/fluxer_api"
			pnpm tsx scripts/CassandraMigrate.tsx --host "$host" --username "$user" --password "$pass" up
		'';
	};

	tasks."licence:check" = {
		exec = ''
			cd "${config.git.root}/fluxer_api"
			pnpm tsx scripts/LicenseEnforcer.tsx
		'';
	};

	tasks."ci:py:sync" = {
		exec = ''
			cd "${config.git.root}/scripts/ci"
			uv sync --dev
		'';
	};

	tasks."ci:py:test" = {
		exec = ''
			cd "${config.git.root}/scripts/ci"
			uv run pytest
		'';
	};

	processes = {
		fluxer_server.exec = "cd ${config.git.root} && pnpm --filter fluxer_server dev";
		fluxer_app.exec = "cd ${config.git.root} && FORCE_COLOR=1 FLUXER_APP_DEV_PORT=49427 pnpm --filter fluxer_app dev";
		marketing_dev.exec = "cd ${config.git.root} && FORCE_COLOR=1 pnpm --filter fluxer_marketing dev";
		css_watch.exec = "cd ${config.git.root} && ${config.git.root}/scripts/dev_css_watch.sh";
		fluxer_gateway.exec = "cd ${config.git.root} && ${config.git.root}/scripts/dev_gateway.sh";
		meilisearch.exec = ''
			MEILI_NO_ANALYTICS=true exec meilisearch \
				--env development \
				--master-key "$(cat ${config.git.root}/dev/meilisearch_master_key 2>/dev/null || true)" \
				--db-path ${config.git.root}/dev/data/meilisearch \
				--http-addr 127.0.0.1:7700
		'';
		livekit.exec = ''
			exec livekit-server --config ${config.git.root}/dev/livekit.yaml
		'';
		mailpit.exec = ''
			exec mailpit --listen 127.0.0.1:49667 --smtp 127.0.0.1:49621 --webroot /mailpit/
		'';
		valkey.exec = "exec valkey-server --bind 127.0.0.1 --port 6379";
		caddy.exec = ''
			exec caddy run --config ${config.git.root}/dev/Caddyfile.dev --adapter caddyfile
		'';
		nats_core.exec = "exec nats-server -p 4222 -a 127.0.0.1";
		nats_jetstream.exec = ''
			exec nats-server -p 4223 -js -sd ${config.git.root}/dev/data/nats_jetstream -a 127.0.0.1
		'';
	};
}
