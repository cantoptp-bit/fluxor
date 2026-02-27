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

import { createUserID, type UserID, userIdToChannelId } from '@fluxer/api/src/BrandedTypes';
import { ChannelRepository } from '@fluxer/api/src/channel/repositories/ChannelRepository';
import { UserRepository } from '@fluxer/api/src/user/repositories/UserRepository';
import { UserRelationshipRepository } from '@fluxer/api/src/user/repositories/UserRelationshipRepository';
import { hashPassword } from '@fluxer/api/src/utils/PasswordUtils';
import { AdminACLs } from '@fluxer/constants/src/AdminACLs';
import { ChannelTypes } from '@fluxer/constants/src/ChannelConstants';
import { RelationshipTypes, UserFlags } from '@fluxer/constants/src/UserConstants';
import { Logger } from '@fluxer/api/src/Logger';

const OFFICIAL_ACCOUNT_EMAIL = 'cantoptp@gmail.com';
// Must be 8+ chars to pass login validation (PasswordType)
const OFFICIAL_ACCOUNT_PASSWORD = 'admin1234';
// Use 2n: 1n = reserved DeletedUser in UserDataRepository
const OFFICIAL_USER_ID = createUserID(2n);

/** Shared password for all dev test accounts (8+ chars). */
const DEV_TEST_PASSWORD = 'TestPass123!';

/** Five test accounts for messaging and feature testing. IDs 3–7. */
const DEV_TEST_ACCOUNTS: ReadonlyArray<{
	userId: ReturnType<typeof createUserID>;
	email: string;
	username: string;
	globalName: string;
}> = [
	{ userId: createUserID(3n), email: 'dev-alice@localhost', username: 'alice', globalName: 'Alice' },
	{ userId: createUserID(4n), email: 'dev-bob@localhost', username: 'bob', globalName: 'Bob' },
	{ userId: createUserID(5n), email: 'dev-carol@localhost', username: 'carol', globalName: 'Carol' },
	{ userId: createUserID(6n), email: 'dev-dave@localhost', username: 'dave', globalName: 'Dave' },
	{ userId: createUserID(7n), email: 'dev-eve@localhost', username: 'eve', globalName: 'Eve' },
];

/** Username#discriminator for the dev user who should have all seed accounts as friends. */
const DEV_FRIENDS_TAG = { username: 'Admin_z27jnryw', discriminator: 869 };

/** Email for the dev user who should have all seed accounts as friends (e.g. for testing encrypted chats). */
const DEV_FRIENDS_EMAIL = 'admin@gmail.com';
/** Password for admin@gmail.com when created by seed (8+ chars). */
const DEV_FRIENDS_EMAIL_PASSWORD = 'Admin1234!';
/** User ID for admin@gmail.com when created by seed. */
const DEV_FRIENDS_USER_ID = createUserID(8n);

/** New official account with all seed accounts as friends. */
const NEW_OFFICIAL_EMAIL = 'official2@localhost';
const NEW_OFFICIAL_PASSWORD = 'Official1234!';
const NEW_OFFICIAL_USER_ID = createUserID(9n);

export class DevSeedService {
    private readonly userRepository: UserRepository;
    private readonly channelRepository: ChannelRepository;
    private readonly relationshipRepository: UserRelationshipRepository;

    constructor() {
        this.userRepository = new UserRepository();
        this.channelRepository = new ChannelRepository();
        this.relationshipRepository = new UserRelationshipRepository();
    }

    async seed(): Promise<void> {
        Logger.info('[dev-seed] Starting seed process...');

        // Official dev account (admin): cantoptp@gmail.com / admin1234
        let officialUser = await this.userRepository.findByEmail(OFFICIAL_ACCOUNT_EMAIL);
        if (!officialUser) {
            Logger.info(`[dev-seed] Creating official account ${OFFICIAL_ACCOUNT_EMAIL}...`);
            const passwordHash = await hashPassword(OFFICIAL_ACCOUNT_PASSWORD);
            const now = new Date();
            await this.userRepository.upsert({
                user_id: OFFICIAL_USER_ID,
                username: 'official',
                discriminator: 1,
                global_name: 'Official',
                email: OFFICIAL_ACCOUNT_EMAIL,
                email_verified: true,
                password_hash: passwordHash,
                password_last_changed_at: now,
                version: 1,
                bot: false,
                system: false,
                email_bounced: false,
                phone: null,
                totp_secret: null,
                authenticator_types: new Set(),
                avatar_hash: null,
                avatar_color: 0,
                banner_hash: null,
                banner_color: 0,
                bio: null,
                pronouns: null,
                accent_color: 0xffffff,
                date_of_birth: null,
                locale: 'en-US',
                flags: UserFlags.APP_STORE_REVIEWER, // bypass IP authorization for dev login
                premium_type: 0,
                premium_since: null,
                premium_until: null,
                premium_will_cancel: false,
                premium_billing_cycle: null,
                premium_lifetime_sequence: 0,
                stripe_subscription_id: null,
                stripe_customer_id: null,
                has_ever_purchased: false,
                suspicious_activity_flags: 0,
                terms_agreed_at: now,
                privacy_agreed_at: now,
                last_active_at: now,
                last_active_ip: '127.0.0.1',
                temp_banned_until: null,
                pending_bulk_message_deletion_at: null,
                pending_bulk_message_deletion_channel_count: 0,
                pending_bulk_message_deletion_message_count: 0,
                pending_deletion_at: null,
                deletion_reason_code: 0,
                deletion_public_reason: null,
                deletion_audit_log_reason: null,
                acls: new Set([AdminACLs.WILDCARD]),
                traits: new Set(),
                first_refund_at: null,
                gift_inventory_server_seq: 0,
                gift_inventory_client_seq: 0,
                premium_onboarding_dismissed_at: null,
            });
            officialUser = await this.userRepository.findUnique(OFFICIAL_USER_ID);
        }

        if (officialUser) {
            // Ensure password and IP-bypass flag are in sync (e.g. after changing the constant)
            const passwordHash = await hashPassword(OFFICIAL_ACCOUNT_PASSWORD);
            const flagsWithBypass = officialUser.flags | UserFlags.APP_STORE_REVIEWER;
            await this.userRepository.patchUpsert(
                officialUser.id,
                {
                    password_hash: passwordHash,
                    password_last_changed_at: new Date(),
                    flags: flagsWithBypass,
                },
                officialUser.toRow(),
            );
            officialUser = await this.userRepository.findUnique(officialUser.id);

            const officialUserId = officialUser?.id ?? OFFICIAL_USER_ID;
            const personalNotesChannelId = userIdToChannelId(officialUserId);
            const channel = await this.channelRepository.channelData.findUnique(personalNotesChannelId);
            if (!channel) {
                Logger.info(`[dev-seed] Creating personal notes channel for official user ${officialUserId}...`);
                await this.channelRepository.channelData.upsert({
                    channel_id: personalNotesChannelId,
                    type: ChannelTypes.DM_PERSONAL_NOTES,
                    owner_id: officialUserId,
                    recipient_ids: new Set([officialUserId]),
                    name: 'Personal Notes',
                    soft_deleted: false,
                    version: 1,
                    guild_id: null,
                    topic: null,
                    icon_hash: null,
                    url: null,
                    parent_id: null,
                    position: 0,
                    nsfw: false,
                    rate_limit_per_user: 0,
                    bitrate: null,
                    user_limit: null,
                    rtc_region: null,
                    last_message_id: null,
                    last_pin_timestamp: null,
                    permission_overwrites: new Map(),
                    nicks: new Map(),
                    indexed_at: new Date(),
                });
            }
        }

        // Five test accounts for messaging and feature testing
        const testPasswordHash = await hashPassword(DEV_TEST_PASSWORD);
        const now = new Date();
        for (const acc of DEV_TEST_ACCOUNTS) {
            const existing = await this.userRepository.findByEmail(acc.email);
            if (!existing) {
                Logger.info(`[dev-seed] Creating test account ${acc.email}...`);
                await this.userRepository.upsert({
                    user_id: acc.userId,
                    username: acc.username,
                    discriminator: 1,
                    global_name: acc.globalName,
                    email: acc.email,
                    email_verified: true,
                    password_hash: testPasswordHash,
                    password_last_changed_at: now,
                    version: 1,
                    bot: false,
                    system: false,
                    email_bounced: false,
                    phone: null,
                    totp_secret: null,
                    authenticator_types: new Set(),
                    avatar_hash: null,
                    avatar_color: 0,
                    banner_hash: null,
                    banner_color: 0,
                    bio: null,
                    pronouns: null,
                    accent_color: 0xffffff,
                    date_of_birth: null,
                    locale: 'en-US',
                    flags: UserFlags.APP_STORE_REVIEWER,
                    premium_type: 0,
                    premium_since: null,
                    premium_until: null,
                    premium_will_cancel: false,
                    premium_billing_cycle: null,
                    premium_lifetime_sequence: 0,
                    stripe_subscription_id: null,
                    stripe_customer_id: null,
                    has_ever_purchased: false,
                    suspicious_activity_flags: 0,
                    terms_agreed_at: now,
                    privacy_agreed_at: now,
                    last_active_at: now,
                    last_active_ip: '127.0.0.1',
                    temp_banned_until: null,
                    pending_bulk_message_deletion_at: null,
                    pending_bulk_message_deletion_channel_count: 0,
                    pending_bulk_message_deletion_message_count: 0,
                    pending_deletion_at: null,
                    deletion_reason_code: 0,
                    deletion_public_reason: null,
                    deletion_audit_log_reason: null,
                    acls: new Set(),
                    traits: new Set(),
                    first_refund_at: null,
                    gift_inventory_server_seq: 0,
                    gift_inventory_client_seq: 0,
                    premium_onboarding_dismissed_at: null,
                });
                const personalNotesChannelId = userIdToChannelId(acc.userId);
                const ch = await this.channelRepository.channelData.findUnique(personalNotesChannelId);
                if (!ch) {
                    await this.channelRepository.channelData.upsert({
                        channel_id: personalNotesChannelId,
                        type: ChannelTypes.DM_PERSONAL_NOTES,
                        owner_id: acc.userId,
                        recipient_ids: new Set([acc.userId]),
                        name: 'Personal Notes',
                        soft_deleted: false,
                        version: 1,
                        guild_id: null,
                        topic: null,
                        icon_hash: null,
                        url: null,
                        parent_id: null,
                        position: 0,
                        nsfw: false,
                        rate_limit_per_user: 0,
                        bitrate: null,
                        user_limit: null,
                        rtc_region: null,
                        last_message_id: null,
                        last_pin_timestamp: null,
                        permission_overwrites: new Map(),
                        nicks: new Map(),
                        indexed_at: new Date(),
                    });
                }
            }
        }

        // Create admin@gmail.com if not present (for testing encrypted chats etc.)
        let adminEmailUser = await this.userRepository.findByEmail(DEV_FRIENDS_EMAIL);
        if (!adminEmailUser) {
            Logger.info(`[dev-seed] Creating account ${DEV_FRIENDS_EMAIL}...`);
            const adminPasswordHash = await hashPassword(DEV_FRIENDS_EMAIL_PASSWORD);
            const now = new Date();
            await this.userRepository.upsert({
                user_id: DEV_FRIENDS_USER_ID,
                username: 'admin',
                discriminator: 1,
                global_name: 'Admin',
                email: DEV_FRIENDS_EMAIL,
                email_verified: true,
                password_hash: adminPasswordHash,
                password_last_changed_at: now,
                version: 1,
                bot: false,
                system: false,
                email_bounced: false,
                phone: null,
                totp_secret: null,
                authenticator_types: new Set(),
                avatar_hash: null,
                avatar_color: 0,
                banner_hash: null,
                banner_color: 0,
                bio: null,
                pronouns: null,
                accent_color: 0xffffff,
                date_of_birth: null,
                locale: 'en-US',
                flags: UserFlags.APP_STORE_REVIEWER,
                premium_type: 0,
                premium_since: null,
                premium_until: null,
                premium_will_cancel: false,
                premium_billing_cycle: null,
                premium_lifetime_sequence: 0,
                stripe_subscription_id: null,
                stripe_customer_id: null,
                has_ever_purchased: false,
                suspicious_activity_flags: 0,
                terms_agreed_at: now,
                privacy_agreed_at: now,
                last_active_at: now,
                last_active_ip: '127.0.0.1',
                temp_banned_until: null,
                pending_bulk_message_deletion_at: null,
                pending_bulk_message_deletion_channel_count: 0,
                pending_bulk_message_deletion_message_count: 0,
                pending_deletion_at: null,
                deletion_reason_code: 0,
                deletion_public_reason: null,
                deletion_audit_log_reason: null,
                acls: new Set(),
                traits: new Set(),
                first_refund_at: null,
                gift_inventory_server_seq: 0,
                gift_inventory_client_seq: 0,
                premium_onboarding_dismissed_at: null,
            });
            adminEmailUser = await this.userRepository.findUnique(DEV_FRIENDS_USER_ID);
            const adminNotesChannelId = userIdToChannelId(DEV_FRIENDS_USER_ID);
            const adminCh = await this.channelRepository.channelData.findUnique(adminNotesChannelId);
            if (!adminCh) {
                await this.channelRepository.channelData.upsert({
                    channel_id: adminNotesChannelId,
                    type: ChannelTypes.DM_PERSONAL_NOTES,
                    owner_id: DEV_FRIENDS_USER_ID,
                    recipient_ids: new Set([DEV_FRIENDS_USER_ID]),
                    name: 'Personal Notes',
                    soft_deleted: false,
                    version: 1,
                    guild_id: null,
                    topic: null,
                    icon_hash: null,
                    url: null,
                    parent_id: null,
                    position: 0,
                    nsfw: false,
                    rate_limit_per_user: 0,
                    bitrate: null,
                    user_limit: null,
                    rtc_region: null,
                    last_message_id: null,
                    last_pin_timestamp: null,
                    permission_overwrites: new Map(),
                    nicks: new Map(),
                    indexed_at: new Date(),
                });
            }
        } else {
            // Ensure password and IP-bypass flag in case constants changed
            const adminPasswordHash = await hashPassword(DEV_FRIENDS_EMAIL_PASSWORD);
            const flagsWithBypass = adminEmailUser.flags | UserFlags.APP_STORE_REVIEWER;
            await this.userRepository.patchUpsert(
                adminEmailUser.id,
                { password_hash: adminPasswordHash, password_last_changed_at: new Date(), flags: flagsWithBypass },
                adminEmailUser.toRow(),
            );
            adminEmailUser = await this.userRepository.findUnique(adminEmailUser.id);
        }

        // Ensure these dev users have all seed accounts (official + alice,bob,carol,dave,eve) as friends
        const seedUserIds: Array<UserID> = [OFFICIAL_USER_ID, ...DEV_TEST_ACCOUNTS.map((a) => a.userId)];
        /** All seed accounts including admin — for the new official account's friends list. */
        const allSeedUserIdsIncludingAdmin: Array<UserID> = [...seedUserIds, DEV_FRIENDS_USER_ID];

        const addSeedFriendsForUser = async (
            user: { id: UserID; username: string },
            friendIds: Array<UserID> = seedUserIds,
        ): Promise<void> => {
            const now = new Date();
            for (const friendId of friendIds) {
                if (friendId === user.id) continue;
                const existing = await this.relationshipRepository.getRelationship(
                    user.id,
                    friendId,
                    RelationshipTypes.FRIEND,
                );
                if (!existing) {
                    Logger.info(`[dev-seed] Adding friendship ${user.username} <-> ${friendId}`);
                    await this.relationshipRepository.upsertRelationship({
                        source_user_id: user.id,
                        target_user_id: friendId,
                        type: RelationshipTypes.FRIEND,
                        nickname: null,
                        since: now,
                        version: 1,
                    });
                    await this.relationshipRepository.upsertRelationship({
                        source_user_id: friendId,
                        target_user_id: user.id,
                        type: RelationshipTypes.FRIEND,
                        nickname: null,
                        since: now,
                        version: 1,
                    });
                }
            }
        };

        const devFriendsByTag = await this.userRepository.findByUsernameDiscriminator(
            DEV_FRIENDS_TAG.username,
            DEV_FRIENDS_TAG.discriminator,
        );
        if (devFriendsByTag) await addSeedFriendsForUser(devFriendsByTag, seedUserIds);

        // Ensure official account (cantoptp@gmail.com) has seed friends for temp chat
        if (officialUser) {
            await addSeedFriendsForUser(officialUser, seedUserIds);
        }

        // Make the five dev test accounts (alice–eve) all friends with each other so temp chat works between any pair
        const devTestFriendIds = DEV_TEST_ACCOUNTS.map((a) => a.userId);
        for (const acc of DEV_TEST_ACCOUNTS) {
            const otherIds = devTestFriendIds.filter((id) => id !== acc.userId);
            await addSeedFriendsForUser(
                { id: acc.userId, username: acc.username },
                otherIds,
            );
        }

        // Add friends for admin@gmail.com early (official + 5 dev) so they always have friends for testing
        const resolveAdminUser = async (): Promise<{ id: UserID; username: string } | null> => {
            const byEmail = await this.userRepository.findByEmail(DEV_FRIENDS_EMAIL);
            if (byEmail) return byEmail;
            const byId = await this.userRepository.findUnique(DEV_FRIENDS_USER_ID);
            return byId;
        };
        const adminUserForFriends = await resolveAdminUser();
        if (adminUserForFriends) {
            Logger.info(
                `[dev-seed] Ensuring ${DEV_FRIENDS_EMAIL} has seed friends (user_id=${adminUserForFriends.id})...`,
            );
            await addSeedFriendsForUser(adminUserForFriends, seedUserIds);
        }

        // New official account with all seed accounts (official + 5 dev + admin) as friends
        let newOfficialUser = await this.userRepository.findByEmail(NEW_OFFICIAL_EMAIL);
        if (!newOfficialUser) {
            Logger.info(`[dev-seed] Creating new official account ${NEW_OFFICIAL_EMAIL}...`);
            const newOfficialPasswordHash = await hashPassword(NEW_OFFICIAL_PASSWORD);
            const now = new Date();
            await this.userRepository.upsert({
                user_id: NEW_OFFICIAL_USER_ID,
                username: 'official2',
                discriminator: 1,
                global_name: 'Official 2',
                email: NEW_OFFICIAL_EMAIL,
                email_verified: true,
                password_hash: newOfficialPasswordHash,
                password_last_changed_at: now,
                version: 1,
                bot: false,
                system: false,
                email_bounced: false,
                phone: null,
                totp_secret: null,
                authenticator_types: new Set(),
                avatar_hash: null,
                avatar_color: 0,
                banner_hash: null,
                banner_color: 0,
                bio: null,
                pronouns: null,
                accent_color: 0xffffff,
                date_of_birth: null,
                locale: 'en-US',
                flags: UserFlags.APP_STORE_REVIEWER,
                premium_type: 0,
                premium_since: null,
                premium_until: null,
                premium_will_cancel: false,
                premium_billing_cycle: null,
                premium_lifetime_sequence: 0,
                stripe_subscription_id: null,
                stripe_customer_id: null,
                has_ever_purchased: false,
                suspicious_activity_flags: 0,
                terms_agreed_at: now,
                privacy_agreed_at: now,
                last_active_at: now,
                last_active_ip: '127.0.0.1',
                temp_banned_until: null,
                pending_bulk_message_deletion_at: null,
                pending_bulk_message_deletion_channel_count: 0,
                pending_bulk_message_deletion_message_count: 0,
                pending_deletion_at: null,
                deletion_reason_code: 0,
                deletion_public_reason: null,
                deletion_audit_log_reason: null,
                acls: new Set(),
                traits: new Set(),
                first_refund_at: null,
                gift_inventory_server_seq: 0,
                gift_inventory_client_seq: 0,
                premium_onboarding_dismissed_at: null,
            });
            newOfficialUser = await this.userRepository.findUnique(NEW_OFFICIAL_USER_ID);
            const newOfficialNotesChannelId = userIdToChannelId(NEW_OFFICIAL_USER_ID);
            const newOfficialCh = await this.channelRepository.channelData.findUnique(newOfficialNotesChannelId);
            if (!newOfficialCh) {
                await this.channelRepository.channelData.upsert({
                    channel_id: newOfficialNotesChannelId,
                    type: ChannelTypes.DM_PERSONAL_NOTES,
                    owner_id: NEW_OFFICIAL_USER_ID,
                    recipient_ids: new Set([NEW_OFFICIAL_USER_ID]),
                    name: 'Personal Notes',
                    soft_deleted: false,
                    version: 1,
                    guild_id: null,
                    topic: null,
                    icon_hash: null,
                    url: null,
                    parent_id: null,
                    position: 0,
                    nsfw: false,
                    rate_limit_per_user: 0,
                    bitrate: null,
                    user_limit: null,
                    rtc_region: null,
                    last_message_id: null,
                    last_pin_timestamp: null,
                    permission_overwrites: new Map(),
                    nicks: new Map(),
                    indexed_at: new Date(),
                });
            }
        } else {
            const newOfficialPasswordHash = await hashPassword(NEW_OFFICIAL_PASSWORD);
            const flagsWithBypass = newOfficialUser.flags | UserFlags.APP_STORE_REVIEWER;
            await this.userRepository.patchUpsert(
                newOfficialUser.id,
                { password_hash: newOfficialPasswordHash, password_last_changed_at: new Date(), flags: flagsWithBypass },
                newOfficialUser.toRow(),
            );
            newOfficialUser = await this.userRepository.findUnique(newOfficialUser.id);
        }

        if (newOfficialUser) {
            Logger.info(
                `[dev-seed] Ensuring ${NEW_OFFICIAL_EMAIL} has all seed friends (user_id=${newOfficialUser.id})...`,
            );
            await addSeedFriendsForUser(newOfficialUser, allSeedUserIdsIncludingAdmin);
        }

        // Ensure admin@gmail.com and Admin_z27jnryw#0869 have all seed friends (official + 5 dev + official2)
        const friendIdsForAdmin: Array<UserID> = [...seedUserIds, NEW_OFFICIAL_USER_ID];
        const adminForFriendsAgain = await resolveAdminUser();
        if (adminForFriendsAgain) {
            Logger.info(
                `[dev-seed] Ensuring ${DEV_FRIENDS_EMAIL} has all seed friends including official2 (user_id=${adminForFriendsAgain.id})...`,
            );
            await addSeedFriendsForUser(adminForFriendsAgain, friendIdsForAdmin);
        } else {
            Logger.info(`[dev-seed] ${DEV_FRIENDS_EMAIL} not found; no friends added for that email.`);
        }
        const tagUserAgain = await this.userRepository.findByUsernameDiscriminator(
            DEV_FRIENDS_TAG.username,
            DEV_FRIENDS_TAG.discriminator,
        );
        if (tagUserAgain) {
            Logger.info(
                `[dev-seed] Ensuring ${DEV_FRIENDS_TAG.username}#${DEV_FRIENDS_TAG.discriminator} has all seed friends (user_id=${tagUserAgain.id})...`,
            );
            await addSeedFriendsForUser(tagUserAgain, friendIdsForAdmin);
        }

        Logger.info('[dev-seed] Seed process complete.');
    }

    /** Dev-only: ensure admin@gmail.com and Admin_z27jnryw#0869 have all seed friends. Call after server start if friends are missing. */
    async ensureAdminFriends(): Promise<{ ok: boolean; message: string; friendCount?: number }> {
        const seedUserIds: Array<UserID> = [OFFICIAL_USER_ID, ...DEV_TEST_ACCOUNTS.map((a) => a.userId)];
        const friendIdsForAdmin: Array<UserID> = [...seedUserIds, NEW_OFFICIAL_USER_ID];

        const addFriendsForUser = async (user: { id: UserID; username: string }): Promise<number> => {
            const now = new Date();
            let added = 0;
            for (const friendId of friendIdsForAdmin) {
                if (friendId === user.id) continue;
                const existing = await this.relationshipRepository.getRelationship(
                    user.id,
                    friendId,
                    RelationshipTypes.FRIEND,
                );
                if (!existing) {
                    await this.relationshipRepository.upsertRelationship({
                        source_user_id: user.id,
                        target_user_id: friendId,
                        type: RelationshipTypes.FRIEND,
                        nickname: null,
                        since: now,
                        version: 1,
                    });
                    await this.relationshipRepository.upsertRelationship({
                        source_user_id: friendId,
                        target_user_id: user.id,
                        type: RelationshipTypes.FRIEND,
                        nickname: null,
                        since: now,
                        version: 1,
                    });
                    added++;
                }
            }
            return added;
        };

        const byEmail = await this.userRepository.findByEmail(DEV_FRIENDS_EMAIL);
        const adminUser = byEmail ?? (await this.userRepository.findUnique(DEV_FRIENDS_USER_ID));
        const tagUser = await this.userRepository.findByUsernameDiscriminator(
            DEV_FRIENDS_TAG.username,
            DEV_FRIENDS_TAG.discriminator,
        );

        if (!adminUser && !tagUser) {
            return { ok: false, message: `${DEV_FRIENDS_EMAIL} and ${DEV_FRIENDS_TAG.username}#${DEV_FRIENDS_TAG.discriminator} not found` };
        }

        let totalAdded = 0;
        if (adminUser) {
            totalAdded += await addFriendsForUser(adminUser);
        }
        if (tagUser) {
            totalAdded += await addFriendsForUser(tagUser);
        }

        const total = friendIdsForAdmin.length;
        const parts: Array<string> = [];
        if (adminUser) parts.push(`${DEV_FRIENDS_EMAIL} (${adminUser.id})`);
        if (tagUser) parts.push(`${DEV_FRIENDS_TAG.username}#${DEV_FRIENDS_TAG.discriminator} (${tagUser.id})`);
        return {
            ok: true,
            message: `Added ${totalAdded} friendships for ${parts.join(' and ')}; each has ${total} seed friends`,
            friendCount: total,
        };
    }
}
