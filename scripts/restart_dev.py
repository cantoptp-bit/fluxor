#!/usr/bin/env python3
"""
Restart Fluxer dev servers: kill processes on dev ports, then start the full stack.
Usage (from repo root):
  python scripts/restart_dev.py           # kill ports, start dev (foreground)
  python scripts/restart_dev.py --bg     # kill ports, start dev in background
  python scripts/restart_dev.py --kill-only   # only free the ports
"""
from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

DEV_PORTS = (49319, 49427, 49531, 48763, 8082)


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent


def kill_ports_windows(root: Path, scripts_dir: Path) -> None:
    ps_script = scripts_dir / "restart_dev_ports.ps1"
    if not ps_script.exists():
        print(f"Warning: {ps_script} not found, skipping port cleanup.", file=sys.stderr)
        return
    subprocess.run(
        [
            "powershell",
            "-ExecutionPolicy",
            "Bypass",
            "-File",
            str(ps_script),
        ],
        cwd=root,
        check=False,
    )


def kill_ports_unix(root: Path) -> None:
    for port in DEV_PORTS:
        try:
            result = subprocess.run(
                ["lsof", "-ti", f":{port}"],
                capture_output=True,
                text=True,
                cwd=root,
            )
            if result.returncode == 0 and result.stdout.strip():
                for pid in result.stdout.strip().split():
                    subprocess.run(
                        ["kill", "-9", pid],
                        capture_output=True,
                        check=False,
                    )
                print(f"Killed process(es) on port {port}")
        except FileNotFoundError:
            pass
    print("Port cleanup done.")


def kill_ports(root: Path) -> None:
    scripts_dir = root / "scripts"
    if sys.platform == "win32":
        kill_ports_windows(root, scripts_dir)
    else:
        kill_ports_unix(root)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Restart Fluxer dev servers: kill dev ports, then start full stack."
    )
    parser.add_argument(
        "--bg",
        action="store_true",
        help="Start dev stack in background and exit.",
    )
    parser.add_argument(
        "--kill-only",
        action="store_true",
        help="Only free the dev ports; do not start the stack.",
    )
    args = parser.parse_args()

    root = repo_root()
    print("Stopping dev ports...")
    kill_ports(root)
    if args.kill_only:
        return 0

    print("Starting dev stack...")
    run_pnpm = ["pnpm", "dev:full"]
    use_shell = sys.platform == "win32"  # so pnpm.cmd is found
    if args.bg:
        subprocess.Popen(
            run_pnpm if not use_shell else "pnpm dev:full",
            cwd=root,
            shell=use_shell,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        print(
            "Dev stack starting in background. Wait ~30s for the app dev server, then open http://localhost:48763"
        )
        return 0

    result = subprocess.run(
        run_pnpm if not use_shell else "pnpm dev:full",
        cwd=root,
        shell=use_shell,
    )
    return result.returncode


if __name__ == "__main__":
    sys.exit(main() or 0)
