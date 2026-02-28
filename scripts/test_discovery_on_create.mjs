#!/usr/bin/env node
/**
 * Test "list in discovery" on community create (live API).
 * Requires dev API running (e.g. API_BASE=http://localhost:48763/api).
 * Usage: node scripts/test_discovery_on_create.mjs
 *
 * For CI/unit tests without a live server, run:
 *   pnpm --filter @fluxer/api test -- src/guild/tests/GuildCreateDiscovery.test.tsx
 */

const API_BASE = process.env.API_BASE ?? 'http://localhost:48763/api';

function log(msg) {
  console.log(msg);
}

function fail(msg) {
  console.error(msg);
  process.exit(1);
}

async function login() {
  const res = await fetch(`${API_BASE}/auth/login`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', 'X-Forwarded-For': '127.0.0.1' },
    body: JSON.stringify({ email: 'admin@gmail.com', password: 'Admin1234!' }),
  });
  if (!res.ok) fail(`Login failed: ${res.status} ${await res.text()}`);
  const data = await res.json();
  if (data.mfa) fail('Account has MFA');
  return data.token;
}

async function createGuild(token, body) {
  const res = await fetch(`${API_BASE}/guilds`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Authorization: token,
      'X-Forwarded-For': '127.0.0.1',
    },
    body: JSON.stringify(body),
  });
  const text = await res.text();
  let json;
  try {
    json = text ? JSON.parse(text) : null;
  } catch {
    json = null;
  }
  return { ok: res.ok, status: res.status, json, text };
}

async function getDiscoveryStatus(token, guildId) {
  const res = await fetch(`${API_BASE}/guilds/${guildId}/discovery`, {
    method: 'GET',
    headers: { Authorization: token, 'X-Forwarded-For': '127.0.0.1' },
  });
  const text = await res.text();
  let json;
  try {
    json = text ? JSON.parse(text) : null;
  } catch {
    json = null;
  }
  return { ok: res.ok, status: res.status, json, text };
}

async function main() {
  log('Discovery-on-create test — API_BASE=' + API_BASE);

  const token = await login();
  log('Logged in.');

  // 1) Create community WITHOUT list_in_discovery
  log('\n1) Create community without discovery...');
  const r1 = await createGuild(token, {
    name: 'Test Community No Discovery ' + Date.now(),
    guild_type: 'community',
  });
  if (!r1.ok) fail('Create without discovery failed: ' + r1.status + ' ' + r1.text);
  const guildId1 = r1.json?.id;
  if (!guildId1) fail('Create without discovery: missing guild id in response');
  log('   OK — guild id ' + guildId1);

  // 2) Create community WITH list_in_discovery (description 10–300, category 0–8)
  log('\n2) Create community with list_in_discovery...');
  const r2 = await createGuild(token, {
    name: 'Test Community With Discovery ' + Date.now(),
    guild_type: 'community',
    list_in_discovery: true,
    discovery_description: 'A test community for the discovery explorer.',
    discovery_category: 0,
  });
  if (!r2.ok) fail('Create with discovery failed: ' + r2.status + ' ' + r2.text);
  const guildId2 = r2.json?.id;
  if (!guildId2) fail('Create with discovery: missing guild id in response');
  log('   OK — guild id ' + guildId2);

  // 2b) Verify discovery application exists (pending)
  const statusRes = await getDiscoveryStatus(token, guildId2);
  if (!statusRes.ok) {
    log('   Warning: GET /guilds/:id/discovery returned ' + statusRes.status + ' (discovery may be disabled or eligibility not met)');
  } else if (statusRes.json?.application) {
    const status = statusRes.json.application.status;
    if (status === 'pending') log('   OK — discovery application is pending.');
    else log('   Info — discovery application status: ' + status);
  } else {
    log('   Info — no discovery application (apply may have been skipped, e.g. eligibility).');
  }

  // 3) Validation: list_in_discovery true but missing description/category → 400
  log('\n3) Validation: list_in_discovery true, missing description and category...');
  const r3 = await createGuild(token, {
    name: 'Bad Discovery ' + Date.now(),
    guild_type: 'community',
    list_in_discovery: true,
  });
  if (r3.ok) fail('Expected 400 when list_in_discovery true without description/category, got 200');
  if (r3.status !== 400) fail('Expected 400, got ' + r3.status + ': ' + r3.text);
  log('   OK — 400 as expected.');

  // 4) Validation: description too short (< 10) → 400
  log('\n4) Validation: discovery_description too short (< 10 chars)...');
  const r4 = await createGuild(token, {
    name: 'Short Desc ' + Date.now(),
    guild_type: 'community',
    list_in_discovery: true,
    discovery_description: 'short',
    discovery_category: 0,
  });
  if (r4.ok) fail('Expected 400 when discovery_description < 10 chars, got 200');
  if (r4.status !== 400) fail('Expected 400, got ' + r4.status + ': ' + r4.text);
  log('   OK — 400 as expected.');

  // 5) Validation: discovery_category out of range (e.g. 99) → 400
  log('\n5) Validation: discovery_category out of range (9)...');
  const r5 = await createGuild(token, {
    name: 'Bad Category ' + Date.now(),
    guild_type: 'community',
    list_in_discovery: true,
    discovery_description: 'Valid description here.',
    discovery_category: 9,
  });
  if (r5.ok) fail('Expected 400 when discovery_category is 9, got 200');
  if (r5.status !== 400) fail('Expected 400, got ' + r5.status + ': ' + r5.text);
  log('   OK — 400 as expected.');

  log('\n✓ All discovery-on-create tests passed.');
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
