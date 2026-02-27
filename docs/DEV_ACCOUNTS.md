# Dev / test accounts

These accounts are created automatically when you run the dev stack (e.g. `pnpm dev:full`) with **development** config and **test mode** enabled. Use them to log in, message each other, and test features.

## Official (admin) account

| Field      | Value                |
|-----------|----------------------|
| **Email** | `cantoptp@gmail.com`  |
| **Password** | `admin1234`      |
| **Username** | `official`        |
| **Note**  | Admin ACLs; use for admin/testing. |

---

## New official (all seed friends)

| Field      | Value                |
|-----------|----------------------|
| **Email** | `official2@localhost` |
| **Password** | `Official1234!`   |
| **Username** | `official2`       |
| **Note**  | Created by seed; has all seed accounts as friends (original official, alice–eve, admin). |

---

## Admin (encrypted chat / feature testing)

| Field      | Value                |
|-----------|----------------------|
| **Email** | `admin@gmail.com`    |
| **Password** | `Admin1234!`     |
| **Username** | `admin`           |
| **Note**  | Created by seed if missing; has all seed accounts as friends for testing DMs/encrypted chats. |

---

## Five test accounts (messaging / feature testing)

All five use the same password. Emails are `@localhost` so they don’t send real mail.

| # | Email | Username | Display name | Password |
|---|--------|----------|----------------|----------|
| 1 | `dev-alice@localhost` | `alice` | Alice | `TestPass123!` |
| 2 | `dev-bob@localhost`   | `bob`   | Bob   | `TestPass123!` |
| 3 | `dev-carol@localhost` | `carol` | Carol | `TestPass123!` |
| 4 | `dev-dave@localhost`  | `dave`  | Dave  | `TestPass123!` |
| 5 | `dev-eve@localhost`  | `eve`   | Eve   | `TestPass123!` |

- **Password (all 5):** `TestPass123!`
- Accounts are **email-verified** and have the dev IP-bypass flag, so you can log in without verification or IP authorization.
- All five are **friends with each other** (and with admin/official2), so you can temp chat between any pair—e.g. log in as alice in one window and bob in another, open temp chat from alice to bob.
- Use them to open multiple sessions (e.g. different browsers or incognito), DM each other, create groups, and test flows.

## When are they created?

On server startup, the **Dev Seed** service runs (see `fluxer_server` + `packages/api/src/dev/DevSeedService.tsx`). It only runs when:

- `config.env === 'development'`
- You’re using the normal dev config (e.g. `config/config.json`)

If an account with that email already exists, it is skipped (so your password changes and profile edits are kept).

### If admin@gmail.com has no friends

The seed adds friends for **admin@gmail.com** (official, alice–eve, official2) on startup. If you still see no friends (e.g. after a DB reset or timing issue), trigger it manually:

```bash
curl -X POST http://localhost:48763/_dev/seed-admin-friends
```

(or `http://localhost:49319/_dev/seed-admin-friends` if not using the proxy). The response will confirm how many friendships were added.

## Quick copy-paste

```
# Official (admin)
cantoptp@gmail.com / admin1234

# New official (all seed friends)
official2@localhost / Official1234!

# Admin (encrypted chat testing)
admin@gmail.com / Admin1234!

# Test accounts (password for all: TestPass123!)
dev-alice@localhost / TestPass123!
dev-bob@localhost   / TestPass123!
dev-carol@localhost / TestPass123!
dev-dave@localhost  / TestPass123!
dev-eve@localhost   / TestPass123!
```
