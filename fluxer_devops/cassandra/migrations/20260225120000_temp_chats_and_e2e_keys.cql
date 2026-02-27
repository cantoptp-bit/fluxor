-- Temporary encrypted chats: one row per friend pair (user_id_1 < user_id_2)
CREATE TABLE IF NOT EXISTS fluxer.temp_chats (
    user_id_1 bigint,
    user_id_2 bigint,
    created_at timestamp,
    PRIMARY KEY ((user_id_1), user_id_2)
);

-- List temp chats by participant: (user_id) -> (user_id_1, user_id_2)
CREATE TABLE IF NOT EXISTS fluxer.temp_chats_by_user (
    user_id bigint,
    user_id_1 bigint,
    user_id_2 bigint,
    PRIMARY KEY ((user_id), user_id_1, user_id_2)
);

-- E2E encrypted messages; partition by chat (user_id_1, user_id_2), order by message_id
CREATE TABLE IF NOT EXISTS fluxer.temp_chat_messages (
    user_id_1 bigint,
    user_id_2 bigint,
    message_id bigint,
    sender_id bigint,
    ciphertext text,
    iv text,
    ephemeral_public_key text,
    created_at timestamp,
    PRIMARY KEY ((user_id_1, user_id_2), message_id)
) WITH CLUSTERING ORDER BY (message_id ASC);

-- Per-user E2E public key (X25519), stored by server; private key stays on client
CREATE TABLE IF NOT EXISTS fluxer.user_e2e_keys (
    user_id bigint PRIMARY KEY,
    public_key_base64 text,
    created_at timestamp
);
