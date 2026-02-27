# Docker Desktop won't start on Windows

If nothing happens when you start Docker Desktop (from the Start menu, taskbar, or the project's **Fluxer: Full Dev** task), try these in order.

## 1. Start it once from PowerShell (visible window)

So you can see any error or splash screen:

```powershell
# Try the usual install path
& "C:\Program Files\Docker\Docker\Docker Desktop.exe"
```

If you get "cannot find path", Docker may be installed elsewhere. Try:

```powershell
& "${env:ProgramFiles}\Docker\Docker\Docker Desktop.exe"
# or
& "${env:ProgramFiles(x86)}\Docker\Docker\Docker Desktop.exe"
```

Wait 1–2 minutes. The whale icon should appear in the system tray and the engine may take a bit longer to become "ready".

---

## 2. Restart your PC

A full reboot often fixes a stuck or half-started Docker daemon.

---

## 3. Kill leftover Docker processes

Sometimes the app "does nothing" because an old process is still running:

1. **Task Manager** (Ctrl+Shift+Esc) → **Details** tab.
2. End any of: `Docker Desktop.exe`, `com.docker.backend`, `com.docker.proxy`, `dockerd`, `docker`.
3. Start Docker Desktop again from the Start menu or with the command above.

---

## 4. Check WSL (if you use WSL 2 backend)

Docker Desktop on Windows often uses WSL 2. If WSL is broken, Docker may not start.

- **Install/repair WSL** (in PowerShell as Administrator):
  ```powershell
  wsl --install
  wsl --update
  ```
- **If Docker was working before and now doesn’t**, you can try unregistering its WSL distros and letting Docker recreate them (this removes Docker’s WSL data):
  ```powershell
  wsl --unregister docker-desktop
  wsl --unregister docker-desktop-data
  ```
  Then start Docker Desktop again (it may re-download).

---

## 5. Enable the WSL engine in Docker settings

If the WSL engine is disabled, Docker can fail to start:

1. Open (or create) `%APPDATA%\Docker\settings.json` (e.g. `C:\Users\YourName\AppData\Roaming\Docker\settings.json`).
2. Ensure you have:
   ```json
   "wslEngineEnabled": true
   ```
3. Save and start Docker Desktop again.

---

## 6. Run Docker Desktop as administrator

Right‑click **Docker Desktop** → **Run as administrator**. If it starts only when elevated, you may need to fix permissions or antivirus (see below).

---

## 7. Antivirus / Windows Defender

Some security software can block Docker or Hyper-V and make it look like "nothing happens":

- Temporarily allow Docker through Windows Defender / your antivirus.
- Add exceptions for:
  - `C:\Program Files\Docker`
  - Docker Desktop and related processes.

---

## 8. Reinstall Docker Desktop

If nothing above works:

1. Uninstall Docker Desktop (Settings → Apps → Docker Desktop → Uninstall).
2. Restart the PC.
3. Download the latest [Docker Desktop for Windows](https://www.docker.com/products/docker-desktop/).
4. Install and choose **WSL 2** when asked, then restart if prompted.
5. Start Docker Desktop and wait until the whale icon shows "Docker Desktop is running".

---

## After Docker is running

Once the tray icon shows Docker is running:

- From the project folder run: **`pnpm dev:full`**, or  
- In the IDE: **Terminal → Run Task… → Fluxer: Full Dev**.

Backing services (including Mailpit for dev emails) will start; open **http://localhost:48763** for the app and **http://localhost:8025** for emails.
