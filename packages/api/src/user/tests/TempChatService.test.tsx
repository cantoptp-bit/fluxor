/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License or (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createUserID} from '@fluxer/api/src/BrandedTypes';
import type {ITempChatRepository} from '@fluxer/api/src/user/repositories/ITempChatRepository';
import {parseTempChatId, tempChatIdFromPair, TempChatService} from '@fluxer/api/src/user/services/TempChatService';
import {describe, expect, it, vi} from 'vitest';

describe('tempChatIdFromPair', () => {
	it('returns canonical id with smaller id first', () => {
		const lo = createUserID(1n);
		const hi = createUserID(2n);
		expect(tempChatIdFromPair(lo, hi)).toBe('1_2');
		expect(tempChatIdFromPair(hi, lo)).toBe('1_2');
	});

	it('returns single segment when both ids equal', () => {
		const id = createUserID(99n);
		expect(tempChatIdFromPair(id, id)).toBe('99_99');
	});
});

describe('parseTempChatId', () => {
	it('parses valid id into [user_id_1, user_id_2]', () => {
		const pair = parseTempChatId('1_2');
		expect(pair).not.toBeNull();
		expect(pair![0]).toBe(1n);
		expect(pair![1]).toBe(2n);
	});

	it('returns null for single segment', () => {
		expect(parseTempChatId('123')).toBeNull();
	});

	it('returns null for more than two segments', () => {
		expect(parseTempChatId('1_2_3')).toBeNull();
	});

	it('returns null when lo >= hi', () => {
		expect(parseTempChatId('2_1')).toBeNull();
		expect(parseTempChatId('5_5')).toBeNull();
	});
});

describe('TempChatService.requestDelete', () => {
	const userId1 = createUserID(100n);
	const userId2 = createUserID(200n);
	const tempChatId = '100_200';

	const baseMockRepo: ITempChatRepository = {
		addDeleteRequest: vi.fn(),
		addDeleteRequestV2: vi.fn(),
		clearDeleteRequests: vi.fn(),
		clearDeleteRequestsV2: vi.fn(),
		createOrGet: vi.fn(),
		createV2: vi.fn(),
		delete: vi.fn(),
		deleteByChatId: vi.fn(),
		getByChatId: vi.fn(),
		getByUserIds: vi.fn(),
		listByUserId: vi.fn(),
		listByUserIdV2: vi.fn(),
		listDeleteRequestUserIds: vi.fn(),
		listDeleteRequestUserIdsV2: vi.fn(),
	};

	it('returns deleted: false when only one user has requested', async () => {
		const mockRepo: ITempChatRepository = {
			...baseMockRepo,
			addDeleteRequest: vi.fn().mockResolvedValue(undefined),
			listDeleteRequestUserIds: vi.fn().mockResolvedValue([userId1]),
		};
		const service = new TempChatService(
			{} as never,
			mockRepo,
			{} as never,
			{} as never,
		);
		const result = await service.requestDelete(userId1, tempChatId);
		expect(result.deleted).toBe(false);
		expect(mockRepo.addDeleteRequest).toHaveBeenCalledWith(userId1, userId2, userId1);
		expect(mockRepo.delete).not.toHaveBeenCalled();
	});

	it('returns deleted: true and deletes when both users have requested', async () => {
		const mockRepo: ITempChatRepository = {
			...baseMockRepo,
			addDeleteRequest: vi.fn().mockResolvedValue(undefined),
			listDeleteRequestUserIds: vi.fn().mockResolvedValue([userId1, userId2]),
			clearDeleteRequests: vi.fn().mockResolvedValue(undefined),
			delete: vi.fn().mockResolvedValue(undefined),
		};
		const service = new TempChatService(
			{} as never,
			mockRepo,
			{} as never,
			{} as never,
		);
		const result = await service.requestDelete(userId2, tempChatId);
		expect(result.deleted).toBe(true);
		expect(mockRepo.clearDeleteRequests).toHaveBeenCalledWith(userId1, userId2);
		expect(mockRepo.delete).toHaveBeenCalledWith(userId1, userId2);
	});

	it('throws TEMP_CHAT_NOT_FOUND for invalid tempChatId', async () => {
		const mockRepo: ITempChatRepository = { ...baseMockRepo };
		const service = new TempChatService(
			{} as never,
			mockRepo,
			{} as never,
			{} as never,
		);
		await expect(service.requestDelete(userId1, 'invalid')).rejects.toThrow('TEMP_CHAT_NOT_FOUND');
		expect(mockRepo.addDeleteRequest).not.toHaveBeenCalled();
	});

	it('throws TEMP_CHAT_FORBIDDEN when user is not a participant', async () => {
		const otherUser = createUserID(999n);
		const mockRepo: ITempChatRepository = { ...baseMockRepo };
		const service = new TempChatService(
			{} as never,
			mockRepo,
			{} as never,
			{} as never,
		);
		await expect(service.requestDelete(otherUser, tempChatId)).rejects.toThrow('TEMP_CHAT_FORBIDDEN');
		expect(mockRepo.addDeleteRequest).not.toHaveBeenCalled();
	});
});
