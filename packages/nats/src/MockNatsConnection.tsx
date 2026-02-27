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

import { EventEmitter } from 'node:events';
import type {
    NatsConnection,
    Msg,
    NatsError,
    Subscription,
    SubscriptionOptions,
    Payload,
    RequestOptions,
    PublishOptions,
    Status,
    Stats,
    ServerInfo,
} from 'nats';
import { StringCodec, JSONCodec } from 'nats';

const GLOBAL_EMITTER_KEY = Symbol.for('fluxer.mock_nats.emitter');

function getGlobalEmitter(): EventEmitter {
    const g = globalThis as any;
    if (!g[GLOBAL_EMITTER_KEY]) {
        g[GLOBAL_EMITTER_KEY] = new EventEmitter();
        g[GLOBAL_EMITTER_KEY].setMaxListeners(0);
    }
    return g[GLOBAL_EMITTER_KEY];
}

const sc = StringCodec();
const jc = JSONCodec();

class MockMsg implements Msg {
    constructor(
        public subject: string,
        public data: Uint8Array,
        public reply?: string,
        public sid: number = 0,
        private emitter: EventEmitter = getGlobalEmitter()
    ) { }

    respond(data?: Payload): boolean {
        if (!this.reply) return false;
        this.emitter.emit(this.reply, new MockMsg(this.reply, data as Uint8Array));
        return true;
    }

    json<T>(): T {
        return jc.decode(this.data) as T;
    }

    string(): string {
        return sc.decode(this.data);
    }
}

class MockSubscription implements Subscription {
    private isClosed_ = false;
    public subject: string;
    public callback: (err: NatsError | null, msg: Msg) => void;

    constructor(subject: string, private emitter: EventEmitter, callback: (err: NatsError | null, msg: Msg) => void) {
        this.subject = subject;
        this.callback = callback;
        this.emitter.on(subject, this.handleMsg.bind(this));
    }

    private handleMsg(msg: Msg) {
        if (this.isClosed_) return;
        this.callback(null, msg);
    }

    async *[Symbol.asyncIterator](): AsyncIterableIterator<Msg> {
        const queue: Msg[] = [];
        let resolve: ((value: IteratorResult<Msg>) => void) | null = null;

        const listener = (msg: Msg) => {
            if (resolve) {
                resolve({ value: msg, done: false });
                resolve = null;
            } else {
                queue.push(msg);
            }
        };

        this.emitter.on(this.subject, listener);

        try {
            while (!this.isClosed_) {
                if (queue.length > 0) {
                    yield queue.shift()!;
                } else {
                    const result = await new Promise<IteratorResult<Msg>>((r) => {
                        resolve = r;
                    });
                    if (result.done) break;
                    yield result.value;
                }
            }
        } finally {
            this.emitter.off(this.subject, listener);
        }
    }

    unsubscribe(_max?: number): void { this.close(); }
    async drain(): Promise<void> { this.close(); return Promise.resolve(); }
    isDraining(): boolean { return false; }
    isClosed(): boolean { return this.isClosed_; }
    getProcessed(): number { return 0; }
    getPending(): number { return 0; }
    getReceived(): number { return 0; }
    getSubject(): string { return this.subject; }
    getID(): number { return 1; }
    getMax(): number | undefined { return undefined; }

    get closed(): Promise<void> {
        return Promise.resolve();
    }

    private close() {
        this.isClosed_ = true;
        this.emitter.off(this.subject, this.handleMsg);
    }
}

export class MockNatsConnection implements Partial<NatsConnection> {
    private emitter = getGlobalEmitter();
    private isClosed_ = false;

    async request(subject: string, data?: Payload, opts?: RequestOptions): Promise<Msg> {
        const replySubject = `_INBOX.${Math.random().toString(36).substring(7)}`;
        const timeout = opts?.timeout ?? 10000;

        return new Promise<Msg>((resolve, reject) => {
            const timer = setTimeout(() => {
                this.emitter.off(replySubject, onResponse);
                reject(new Error('TIMEOUT'));
            }, timeout);

            const onResponse = (msg: Msg) => {
                clearTimeout(timer);
                this.emitter.off(replySubject, onResponse);
                resolve(msg);
            };

            this.emitter.on(replySubject, onResponse);
            this.emitter.emit(subject, new MockMsg(subject, data as Uint8Array, replySubject));
        });
    }

    publish(subject: string, data?: Payload, _opts?: PublishOptions): void {
        this.emitter.emit(subject, new MockMsg(subject, data as Uint8Array));
    }

    subscribe(subject: string, opts?: SubscriptionOptions): Subscription {
        const callback = opts?.callback ?? (() => {});
        return new MockSubscription(subject, this.emitter, callback);
    }

    async drain(): Promise<void> {
        this.isClosed_ = true;
        return Promise.resolve();
    }

    async close(): Promise<void> {
        this.isClosed_ = true;
        return Promise.resolve();
    }

    isClosed(): boolean { return this.isClosed_; }
    isDraining(): boolean { return false; }

    status(): AsyncIterable<Status> {
        return (async function* () {
            yield { type: 'reconnect' as any, data: '' } as Status;
        })();
    }

    async flush(): Promise<void> { return Promise.resolve(); }
    stats(): Stats { return {} as Stats; }
    getServerInfo(): ServerInfo | undefined { return { server_id: 'mock', server_name: 'mock', version: '0.0.0' } as ServerInfo; }

    closed(): Promise<void | Error> {
        return Promise.resolve();
    }
}
