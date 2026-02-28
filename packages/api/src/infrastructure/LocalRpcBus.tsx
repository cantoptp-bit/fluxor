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

export type LocalRpcResponder = (params: Record<string, unknown>) => Promise<{ ok: boolean; result?: unknown; error?: string }>;

const GLOBAL_BUS_KEY = 'FLUXER_LOCAL_RPC_BUS';

export class LocalRpcBus {
    private readonly responders = new Map<string, LocalRpcResponder>();

    private constructor() { }

    static getInstance(): LocalRpcBus {
        const g = globalThis as any;
        if (!g[GLOBAL_BUS_KEY]) {
            console.error(`[local-rpc] [PID:${process.pid}] Creating new global LocalRpcBus instance`);
            g[GLOBAL_BUS_KEY] = new LocalRpcBus();
        }
        return g[GLOBAL_BUS_KEY];
    }

    register(subject: string, responder: LocalRpcResponder): void {
        console.error(`[local-rpc] [PID:${process.pid}] Registering responder for ${subject}`);
        this.responders.set(subject, responder);
    }

    async call(subject: string, params: Record<string, unknown>): Promise<unknown> {
        const responder = this.responders.get(subject);
        if (!responder) {
            console.error(`[local-rpc] [PID:${process.pid}] No responder for ${subject}`);
            throw new Error(`[local-rpc] No responder found for subject: ${subject}`);
        }

        console.error(`[local-rpc] [PID:${process.pid}] Calling local responder for ${subject}`);
        const response = await responder(params);

        if (!response.ok) {
            throw new Error(response.error ?? 'INTERNAL_ERROR');
        }

        return response.result;
    }

    hasResponder(subject: string): boolean {
        const has = this.responders.has(subject);
        console.error(`[local-rpc] [PID:${process.pid}] Checking for responder ${subject}: ${has}`);
        return has;
    }
}
