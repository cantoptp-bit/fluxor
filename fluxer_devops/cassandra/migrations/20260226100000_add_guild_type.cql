-- Add guild_type to support Create Server vs Create Community (values: 'server', 'community').
ALTER TABLE fluxer.guilds ADD guild_type text;
