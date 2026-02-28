#!/usr/bin/env node
/**
 * Test sending a message in a community channel.
 * 1. Login
 * 2. Call gateway session-init (so HAS_SESSION_STARTED is set – same as when app connects to gateway)
 * 3. Get or create a guild and pick a channel
 * 4. POST a message to that channel
 * Requires dev servers running (pnpm dev:servers). Uses admin account by default.
 * Usage: node scripts/test_send_message.mjs
 */

const API_BASE = process.env.API_BASE ?? 'http://localhost:48763/api';

const headers = (token) => ({
  'Content-Type': 'application/json',
  'X-Forwarded-For': '127.0.0.1',
  ...(token ? { Authorization: token } : {}),
});

async function main() {
  console.log('Send message test – using', API_BASE);

  // 1. Login
  const loginRes = await fetch(`${API_BASE}/auth/login`, {
    method: 'POST',
    headers: headers(null),
    body: JSON.stringify({
      email: process.env.TEST_EMAIL ?? 'admin@gmail.com',
      password: process.env.TEST_PASSWORD ?? 'Admin1234!',
    }),
  });

  if (!loginRes.ok) {
    console.error('Login failed:', loginRes.status, await loginRes.text());
    process.exit(1);
  }

  const login = await loginRes.json();
  if (login.mfa) {
    console.error('Account has MFA – use TEST_EMAIL/TEST_PASSWORD without MFA');
    process.exit(1);
  }

  const token = login.token;
  console.log('Logged in');

  // 2. Gateway session-init (same as when the app connects to gateway and sends IDENTIFY)
  const sessionInitRes = await fetch(`${API_BASE}/gateway/session-init`, {
    method: 'POST',
    headers: headers(token),
    body: JSON.stringify({ type: 'session', token, version: 1 }),
  });

  if (!sessionInitRes.ok) {
    const text = await sessionInitRes.text();
    console.error('Session init failed:', sessionInitRes.status, text);
    process.exit(1);
  }
  console.log('Gateway session-init OK (HAS_SESSION_STARTED set)');

  // 3. Get user's guilds
  const guildsRes = await fetch(`${API_BASE}/users/@me/guilds`, {
    headers: headers(token),
  });
  if (!guildsRes.ok) {
    console.error('List guilds failed:', guildsRes.status, await guildsRes.text());
    process.exit(1);
  }
  const guilds = await guildsRes.json();
  let guildId = guilds[0]?.id;
  if (!guildId) {
    console.log('No guilds – creating one...');
    const createRes = await fetch(`${API_BASE}/guilds`, {
      method: 'POST',
      headers: headers(token),
      body: JSON.stringify({ name: 'Test Community' }),
    });
    if (!createRes.ok) {
      console.error('Create guild failed:', createRes.status, await createRes.text());
      process.exit(1);
    }
    const guild = await createRes.json();
    guildId = guild.id;
    console.log('Created guild:', guildId);
  } else {
    console.log('Using guild:', guildId);
  }

  // 4. Get channels for the guild
  const channelsRes = await fetch(`${API_BASE}/guilds/${guildId}/channels`, {
    headers: headers(token),
  });
  if (!channelsRes.ok) {
    console.error('List channels failed:', channelsRes.status, await channelsRes.text());
    process.exit(1);
  }
  let channels = await channelsRes.json();
  // If this guild has no channels, create a fresh guild (has default channels)
  if (!channels.length) {
    console.log('Guild has no channels – creating a new guild with default channels...');
    const createRes = await fetch(`${API_BASE}/guilds`, {
      method: 'POST',
      headers: headers(token),
      body: JSON.stringify({ name: 'Test Community ' + Date.now() }),
    });
    if (!createRes.ok) {
      console.error('Create guild failed:', createRes.status, await createRes.text());
      process.exit(1);
    }
    const guild = await createRes.json();
    guildId = guild.id;
    const chRes = await fetch(`${API_BASE}/guilds/${guildId}/channels`, { headers: headers(token) });
    if (!chRes.ok) {
      console.error('List channels failed:', chRes.status, await chRes.text());
      process.exit(1);
    }
    channels = await chRes.json();
  }
  // GUILD_TEXT = 0 – pick first text channel so we can send a message
  const textChannel = channels.find((c) => c.type === 0) ?? channels[0];
  const channelId = textChannel?.id;
  if (!channelId) {
    console.error('No channels in guild – create a channel in the app first');
    process.exit(1);
  }
  console.log('Using channel:', channelId, textChannel?.type === 0 ? '(text)' : '(first available)');

  // 5. Send message
  const messageBody = { content: 'Test message from script – ' + new Date().toISOString() };
  const sendRes = await fetch(`${API_BASE}/channels/${channelId}/messages`, {
    method: 'POST',
    headers: headers(token),
    body: JSON.stringify(messageBody),
  });

  const sendText = await sendRes.text();
  if (!sendRes.ok) {
    console.error('Send message failed:', sendRes.status, sendText);
    process.exit(1);
  }

  const message = JSON.parse(sendText);
  console.log('Message sent. id:', message.id, 'content:', message.content?.slice(0, 50));
  console.log('\n✓ Send message test passed');
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
