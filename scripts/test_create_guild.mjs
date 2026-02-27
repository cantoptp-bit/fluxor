#!/usr/bin/env node
/**
 * Test guild/community creation. Requires dev servers running.
 * Usage: node scripts/test_create_guild.mjs
 */

const API_BASE = process.env.API_BASE ?? 'http://localhost:48763/api';

async function main() {
  console.log('Guild create test - using', API_BASE);

  const loginRes = await fetch(`${API_BASE}/auth/login`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'X-Forwarded-For': '127.0.0.1',
    },
    body: JSON.stringify({
      email: 'admin@gmail.com',
      password: 'Admin1234!',
    }),
  });

  if (!loginRes.ok) {
    console.error('Login failed:', loginRes.status, await loginRes.text());
    process.exit(1);
  }

  const login = await loginRes.json();
  if (login.mfa) {
    console.error('Account has MFA');
    process.exit(1);
  }

  const token = login.token;
  console.log('Logged in, creating guild...');

  const createRes = await fetch(`${API_BASE}/guilds`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Authorization: token,
      'X-Forwarded-For': '127.0.0.1',
    },
    body: JSON.stringify({ name: 'Test Community' }),
  });

  const text = await createRes.text();
  console.log('Status:', createRes.status);
  console.log('Response:', text.slice(0, 500));

  if (!createRes.ok) {
    process.exit(1);
  }
  console.log('\n✓ Guild create test passed');
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
