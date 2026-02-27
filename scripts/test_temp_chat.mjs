#!/usr/bin/env node
/**
 * Test temp chat API: login as admin, create temp chat with a friend (alice).
 * Requires dev servers running (pnpm dev:servers) and seed accounts.
 * Usage: node scripts/test_temp_chat.mjs
 */

const API_BASE = process.env.API_BASE ?? 'http://localhost:48763/api';

async function main() {
  console.log('Temp chat test - using', API_BASE);

  // 1. Login as admin (has alice as friend)
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
    const text = await loginRes.text();
    console.error('Login failed:', loginRes.status, text);
    process.exit(1);
  }

  const login = await loginRes.json();
  if (login.mfa) {
    console.error('Admin account has MFA - use a test account without MFA');
    process.exit(1);
  }

  const token = login.token;
  const userId = login.user_id;
  console.log('Logged in as user_id:', userId);

  // 2. Create temp chat with alice (user id 3 from dev seed)
  const recipientId = '3';
  const createRes = await fetch(`${API_BASE}/users/@me/temp-chats`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Authorization: token,
      'X-Forwarded-For': '127.0.0.1',
    },
    body: JSON.stringify({ recipient_id: recipientId }),
  });

  if (!createRes.ok) {
    const text = await createRes.text();
    console.error('Create temp chat failed:', createRes.status, text);
    process.exit(1);
  }

  const chat = await createRes.json();
  console.log('Temp chat created:', JSON.stringify(chat, null, 2));

  // 3. List temp chats
  const listRes = await fetch(`${API_BASE}/users/@me/temp-chats`, {
    headers: {
      Authorization: token,
      'X-Forwarded-For': '127.0.0.1',
    },
  });
  if (listRes.ok) {
    const list = await listRes.json();
    console.log('Temp chats list:', list.length, 'chat(s)');
  }

  console.log('\n✓ Temp chat test passed');
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
