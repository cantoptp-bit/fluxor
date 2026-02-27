-- V2 temp chats: multiple chats per user pair, identified by snowflake chat_id
CREATE TABLE IF NOT EXISTS fluxer.temp_chats_v2 (
    chat_id bigint,
    user_id_1 bigint,
    user_id_2 bigint,
    created_at timestamp,
    PRIMARY KEY (chat_id)
);

-- List V2 temp chats by participant: (user_id) -> (chat_id, user_id_1, user_id_2, created_at)
CREATE TABLE IF NOT EXISTS fluxer.temp_chats_by_user_v2 (
    user_id bigint,
    chat_id bigint,
    user_id_1 bigint,
    user_id_2 bigint,
    created_at timestamp,
    PRIMARY KEY ((user_id), chat_id)
) WITH CLUSTERING ORDER BY (chat_id ASC);

-- E2E encrypted messages for V2 chats; partition by chat_id, order by message_id
CREATE TABLE IF NOT EXISTS fluxer.temp_chat_messages_v2 (
    chat_id bigint,
    message_id bigint,
    sender_id bigint,
    ciphertext text,
    iv text,
    ephemeral_public_key text,
    created_at timestamp,
    PRIMARY KEY ((chat_id), message_id)
) WITH CLUSTERING ORDER BY (message_id ASC);

-- Delete requests for V2 chats; both participants must have a row to actually delete
CREATE TABLE IF NOT EXISTS fluxer.temp_chat_delete_requests_v2 (
    chat_id bigint,
    user_id bigint,
    PRIMARY KEY ((chat_id), user_id)
);
