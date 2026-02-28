/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type { IKVPipeline, IKVProvider, IKVSubscription } from '@fluxer/kv_client/src/IKVProvider';

export class MockKVSubscription implements IKVSubscription {
    private messageCallbacks: Array<(channel: string, message: string) => void> = [];

    async connect(): Promise<void> { }
    on(
        event: 'message' | 'error',
        callback: ((channel: string, message: string) => void) | ((error: Error) => void),
    ): void {
        if (event === 'message') this.messageCallbacks.push(callback as (channel: string, message: string) => void);
    }
    async subscribe(..._channels: Array<string>): Promise<void> { }
    async unsubscribe(..._channels: Array<string>): Promise<void> { }
    async quit(): Promise<void> { }
    async disconnect(): Promise<void> { }
    removeAllListeners(event?: 'message' | 'error'): void {
        if (event === 'message' || !event) this.messageCallbacks = [];
    }
}

class MockKVPipeline implements IKVPipeline {
    private operations: Array<() => Promise<unknown>> = [];

    constructor(private provider: MockKVProvider) { }

    get(key: string) { this.operations.push(() => this.provider.get(key)); return this; }
    set(key: string, value: string) { this.operations.push(() => this.provider.set(key, value)); return this; }
    setex(key: string, ttlSeconds: number, value: string) { this.operations.push(() => this.provider.setex(key, ttlSeconds, value)); return this; }
    del(key: string) { this.operations.push(() => this.provider.del(key)); return this; }
    expire(key: string, ttlSeconds: number) { this.operations.push(() => this.provider.expire(key, ttlSeconds)); return this; }
    sadd(key: string, ...members: Array<string>) { this.operations.push(() => this.provider.sadd(key, ...members)); return this; }
    srem(key: string, ...members: Array<string>) { this.operations.push(() => this.provider.srem(key, ...members)); return this; }
    zadd(key: string, score: number, value: string) { this.operations.push(() => this.provider.zadd(key, score, value)); return this; }
    zrem(key: string, ...members: Array<string>) { this.operations.push(() => this.provider.zrem(key, ...members)); return this; }
    mset(...args: Array<string>) { this.operations.push(() => this.provider.mset(...args)); return this; }

    async exec(): Promise<Array<[Error | null, unknown]>> {
        const results: Array<[Error | null, unknown]> = [];
        for (const op of this.operations) {
            try {
                results.push([null, await op()]);
            } catch (err) {
                results.push([err instanceof Error ? err : new Error(String(err)), null]);
            }
        }
        return results;
    }
}

export class MockKVProvider implements IKVProvider {
    private readonly store = new Map<string, string>();
    private readonly hashes = new Map<string, Map<string, string>>();

    async get(key: string): Promise<string | null> {
        return this.store.get(key) ?? null;
    }
    async set(key: string, value: string): Promise<string | null> {
        this.store.set(key, value);
        return 'OK';
    }
    async setex(key: string, _ttl: number, value: string): Promise<void> {
        this.store.set(key, value);
    }
    async setnx(key: string, value: string): Promise<boolean> {
        if (this.store.has(key)) return false;
        this.store.set(key, value);
        return true;
    }
    async mget(...keys: Array<string>): Promise<Array<string | null>> {
        return keys.map((k) => this.store.get(k) ?? null);
    }
    async mset(...args: Array<string>): Promise<void> {
        for (let i = 0; i < args.length; i += 2) {
            this.store.set(args[i], args[i + 1]);
        }
    }
    async del(...keys: Array<string>): Promise<number> {
        let count = 0;
        for (const k of keys) {
            if (this.store.delete(k)) count++;
        }
        return count;
    }
    async exists(key: string): Promise<number> {
        return this.store.has(key) ? 1 : 0;
    }
    async expire(_key: string, _ttl: number): Promise<number> {
        return 1;
    }
    async ttl(_key: string): Promise<number> {
        return -1;
    }
    async incr(key: string): Promise<number> {
        const val = Number.parseInt(this.store.get(key) ?? '0', 10) + 1;
        this.store.set(key, val.toString());
        return val;
    }
    async getex(key: string, _ttl: number): Promise<string | null> {
        return this.get(key);
    }
    async getdel(key: string): Promise<string | null> {
        const val = await this.get(key);
        this.store.delete(key);
        return val;
    }

    async sadd(_key: string, ..._members: Array<string>): Promise<number> { return 1; }
    async srem(_key: string, ..._members: Array<string>): Promise<number> { return 1; }
    async smembers(_key: string): Promise<Array<string>> { return []; }
    async sismember(_key: string, _member: string): Promise<number> { return 0; }
    async scard(_key: string): Promise<number> { return 0; }
    async spop(_key: string, _count?: number): Promise<Array<string>> { return []; }

    async zadd(_key: string, ..._scoreMembers: Array<number | string>): Promise<number> { return 1; }
    async zrem(_key: string, ..._members: Array<string>): Promise<number> { return 1; }
    async zcard(_key: string): Promise<number> { return 0; }
    async zrangebyscore(_key: string, _min: string | number, _max: string | number): Promise<Array<string>> { return []; }

    async rpush(_key: string, ..._values: Array<string>): Promise<number> { return 1; }
    async lpop(_key: string, _count?: number): Promise<Array<string>> { return []; }
    async llen(_key: string): Promise<number> { return 0; }

    async hset(key: string, field: string, value: string): Promise<number> {
        if (!this.hashes.has(key)) this.hashes.set(key, new Map());
        this.hashes.get(key)!.set(field, value);
        return 1;
    }
    async hdel(key: string, ...fields: Array<string>): Promise<number> {
        const hash = this.hashes.get(key);
        if (!hash) return 0;
        let count = 0;
        for (const f of fields) if (hash.delete(f)) count++;
        return count;
    }
    async hget(key: string, field: string): Promise<string | null> {
        return this.hashes.get(key)?.get(field) ?? null;
    }
    async hgetall(key: string): Promise<Record<string, string>> {
        const hash = this.hashes.get(key);
        if (!hash) return {};
        return Object.fromEntries(hash.entries());
    }

    async publish(_channel: string, _message: string): Promise<number> { return 1; }
    duplicate(): IKVSubscription { return new MockKVSubscription(); }

    async releaseLock(_key: string, _token: string): Promise<boolean> { return true; }
    async renewSnowflakeNode(_key: string, _instanceId: string, _ttl: number): Promise<boolean> { return true; }
    async tryConsumeTokens(_key: string, requested: number): Promise<number> { return requested; }
    async scheduleBulkDeletion(_q: string, _s: string, _sc: number, _v: string): Promise<void> { }
    async removeBulkDeletion(_q: string, _s: string): Promise<boolean> { return true; }
    async scan(_pattern: string, _count: number): Promise<Array<string>> { return []; }

    pipeline(): IKVPipeline { return new MockKVPipeline(this); }
    multi(): IKVPipeline { return new MockKVPipeline(this); }
    async health(): Promise<boolean> { return true; }
}
