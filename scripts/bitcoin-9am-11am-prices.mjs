#!/usr/bin/env node
/**
 * Fetches Bitcoin prices for the past year at ~6 AM and ~6 PM (UTC).
 * Uses CoinGecko free API; requests 90-day chunks to get hourly data.
 *
 * Usage: node scripts/bitcoin-9am-11am-prices.mjs
 * Optional: TZ=America/New_York node scripts/bitcoin-9am-11am-prices.mjs (for local 6 AM / 6 PM)
 */

const TZ_OFFSET_HOURS = 0; // 0 = UTC. Set e.g. -5 for EST so 9 AM local = 14 UTC.
const DELAY_BETWEEN_CHUNKS_MS = 15_000; // CoinGecko free tier ~10–15 req/min; wait 15s between chunks
const MAX_RETRIES = 5;
const RETRY_BASE_MS = 60_000; // wait 1 min on first 429, then 2 min, 4 min, etc.

async function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

async function fetchRange(from, to) {
  const url = `https://api.coingecko.com/api/v3/coins/bitcoin/market_chart/range?vs_currency=usd&from=${from}&to=${to}`;
  for (let attempt = 1; attempt <= MAX_RETRIES; attempt++) {
    const res = await fetch(url);
    if (res.ok) return res.json();
    const text = await res.text();
    if (res.status === 429 && attempt < MAX_RETRIES) {
      const waitMs = RETRY_BASE_MS * Math.pow(2, attempt - 1);
      console.error(`Rate limited (429). Waiting ${Math.round(waitMs / 1000)}s before retry ${attempt}/${MAX_RETRIES}...`);
      await sleep(waitMs);
      continue;
    }
    throw new Error(`API ${res.status}: ${text}`);
  }
}

function toLocalDay(tsMs, offsetHours = TZ_OFFSET_HOURS) {
  const d = new Date(tsMs + offsetHours * 60 * 60 * 1000);
  return `${d.getUTCFullYear()}-${String(d.getUTCMonth() + 1).padStart(2, "0")}-${String(d.getUTCDate()).padStart(2, "0")}`;
}

function localHour(tsMs, offsetHours = TZ_OFFSET_HOURS) {
  const d = new Date(tsMs + offsetHours * 60 * 60 * 1000);
  return d.getUTCHours() + d.getUTCMinutes() / 60;
}

function groupByDay(prices) {
  const byDay = {};
  for (const [ts, price] of prices) {
    const day = toLocalDay(ts);
    if (!byDay[day]) byDay[day] = [];
    byDay[day].push([ts, price]);
  }
  return byDay;
}

function pickClosest(points, targetHour) {
  let best = null;
  let bestDiff = Infinity;
  for (const [ts, price] of points) {
    const h = localHour(ts);
    const diff = Math.abs(h - targetHour);
    if (diff < bestDiff) {
      bestDiff = diff;
      best = { ts, price, hour: h };
    }
  }
  return best;
}

function pickAfter(points, afterHour) {
  const candidates = points.filter(([ts]) => localHour(ts) >= afterHour);
  if (candidates.length === 0) return null;
  const [ts, price] = candidates.reduce((a, b) => (a[0] < b[0] ? a : b));
  return { ts, price, hour: localHour(ts) };
}

async function main() {
  const now = Math.floor(Date.now() / 1000);
  const oneYearAgo = now - 365 * 24 * 60 * 60;
  const chunkDays = 90;
  const chunkSec = chunkDays * 24 * 60 * 60;
  const allPrices = [];

  console.error("Fetching Bitcoin hourly data from CoinGecko (4 x 90-day chunks)...");
  console.error(`Waiting ${DELAY_BETWEEN_CHUNKS_MS / 1000}s between requests to respect rate limits.`);
  let chunkIndex = 0;
  for (let from = oneYearAgo; from < now; from += chunkSec) {
    chunkIndex++;
    if (chunkIndex > 1) await sleep(DELAY_BETWEEN_CHUNKS_MS);
    const to = Math.min(from + chunkSec, now);
    console.error(`Chunk ${chunkIndex}/4...`);
    const data = await fetchRange(from, to);
    allPrices.push(...data.prices);
  }

  const byDay = groupByDay(allPrices);
  const days = Object.keys(byDay).sort();
  const rows = [];

  for (const day of days) {
    const points = byDay[day];
    const at6am = pickClosest(points, 6);
    const at6pm = pickAfter(points, 18) || pickClosest(points, 18);
    if (!at6am || !at6pm) continue;
    rows.push({
      date: day,
      priceAt6am: at6am.price,
      priceAt6pm: at6pm.price,
      changePct: (((at6pm.price - at6am.price) / at6am.price) * 100).toFixed(2) + "%",
    });
  }

  console.log("\nBitcoin (USD) – past year: price at ~6 AM and ~6 PM (UTC)\n");
  console.log("Date         |  Price ~6 AM  |  Price ~6 PM        |  Change (6→18)");
  console.log("-------------|---------------|---------------------|----------------");
  for (const r of rows) {
    console.log(
      `${r.date} | ${String(r.priceAt6am).padStart(13)} | ${String(r.priceAt6pm).padStart(19)} | ${r.changePct}`
    );
  }

  const up = rows.filter((r) => r.priceAt6pm > r.priceAt6am).length;
  const down = rows.filter((r) => r.priceAt6pm < r.priceAt6am).length;
  const unchanged = rows.filter((r) => r.priceAt6pm === r.priceAt6am).length;
  const total = rows.length;

  console.log("\n--- Stats (6 AM → 6 PM UTC) ---");
  console.log(`Total days:    ${total}`);
  console.log(`Up:            ${up} (${((up / total) * 100).toFixed(1)}%)`);
  console.log(`Down:          ${down} (${((down / total) * 100).toFixed(1)}%)`);
  if (unchanged > 0) console.log(`Unchanged:     ${unchanged} (${((unchanged / total) * 100).toFixed(1)}%)`);

  console.log("\n(Timezone: UTC. Set TZ_OFFSET_HOURS in the script for local 6 AM / 6 PM.)");
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
