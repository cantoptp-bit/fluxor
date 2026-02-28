import { readFileSync, writeFileSync, existsSync } from 'node:fs';
import { randomBytes, generateKeyPairSync } from 'node:crypto';
import { join } from 'node:path';

const REPO_ROOT = process.cwd();
const configPath = process.env.FLUXER_CONFIG || join(REPO_ROOT, 'config.json');

if (!existsSync(configPath)) {
    console.error(`Config file not found: ${configPath}`);
    process.exit(1);
}

const config = JSON.parse(readFileSync(configPath, 'utf8'));

function generateHex(bytes) {
    return randomBytes(bytes).toString('hex');
}

function isPlaceholder(value) {
    if (!value || typeof value !== 'string') return true;
    const placeholders = ['GENERATE_', 'YOUR_', 'PLACEHOLDER', 'ADD_YOUR'];
    return placeholders.some(p => value.includes(p)) || value.length < 8;
}

// 1. Core Secrets
if (isPlaceholder(config.auth?.connection_initiation_secret)) {
    console.log('Generating connection_initiation_secret...');
    config.auth = config.auth || {};
    config.auth.connection_initiation_secret = generateHex(32);
}

if (isPlaceholder(config.auth?.sudo_mode_secret)) {
    console.log('Generating sudo_mode_secret...');
    config.auth = config.auth || {};
    config.auth.sudo_mode_secret = generateHex(32);
}

if (isPlaceholder(config.services?.media_proxy?.secret_key)) {
    console.log('Generating media_proxy secret_key...');
    config.services.media_proxy.secret_key = generateHex(32);
}

if (isPlaceholder(config.services?.admin?.secret_key_base)) {
    console.log('Generating admin secret_key_base...');
    config.services.admin.secret_key_base = generateHex(32);
}

if (isPlaceholder(config.services?.admin?.oauth_client_secret)) {
    console.log('Generating admin oauth_client_secret...');
    config.services.admin.oauth_client_secret = generateHex(32);
}

if (isPlaceholder(config.services?.marketing?.secret_key_base)) {
    console.log('Generating marketing secret_key_base...');
    config.services.marketing.secret_key_base = generateHex(32);
}

if (isPlaceholder(config.services?.gateway?.admin_reload_secret)) {
    console.log('Generating gateway admin_reload_secret...');
    config.services.gateway.admin_reload_secret = generateHex(32);
}

if (isPlaceholder(config.services?.nats?.auth_token)) {
    console.log('Generating NATS auth_token...');
    config.services.nats.auth_token = generateHex(16);
}

// 2. VAPID Keys
function generateVapidKeys() {
    const { publicKey, privateKey } = generateKeyPairSync('ec', { namedCurve: 'prime256v1' });
    const publicJwk = publicKey.export({ format: 'jwk' });
    const privateJwk = privateKey.export({ format: 'jwk' });
    const publicRaw = Buffer.concat([
        Buffer.from([0x04]),
        Buffer.from(publicJwk.x, 'base64url'),
        Buffer.from(publicJwk.y, 'base64url'),
    ]);
    return {
        public_key: publicRaw.toString('base64url'),
        private_key: privateJwk.d,
    };
}

if (isPlaceholder(config.auth?.vapid?.public_key) || isPlaceholder(config.auth?.vapid?.private_key)) {
    console.log('Generating VAPID keys...');
    config.auth.vapid = { ...config.auth.vapid, ...generateVapidKeys() };
}

// 3. Infrastructure & Integrations
if (isPlaceholder(config.integrations?.search?.api_key)) {
    console.log('Generating Meilisearch api_key...');
    config.integrations.search.api_key = generateHex(32);
}

// 4. Mock S3 and other dev placeholders if empty
if (isPlaceholder(config.s3?.access_key_id)) {
    config.s3 = config.s3 || {};
    config.s3.access_key_id = 'dev-access-key';
}
if (isPlaceholder(config.s3?.secret_access_key)) {
    config.s3 = config.s3 || {};
    config.s3.secret_access_key = generateHex(32);
}

writeFileSync(configPath, JSON.stringify(config, null, 2));
console.log(`Successfully updated ${configPath} with unique secrets.`);
