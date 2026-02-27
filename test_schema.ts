import { WellKnownFluxerResponse } from "./packages/schema/src/domains/instance/InstanceSchemas.tsx";
import { z } from "zod";

const payload = {
    api_code_version: 1,
    endpoints: {
        api: "http://localhost:49319/api",
        api_client: "http://localhost:49319/api",
        api_public: "http://localhost:49319/api",
        gateway: "ws://localhost:49319",
        media: "http://localhost:49319",
        static_cdn: "http://localhost:49319",
        marketing: "http://localhost:49319",
        admin: "http://localhost:49319",
        invite: "http://localhost:49319",
        gift: "http://localhost:49319",
        webapp: "http://localhost:49427"
    },
    captcha: {
        provider: "none",
        hcaptcha_site_key: null,
        turnstile_site_key: null
    },
    features: {
        sms_mfa_enabled: false,
        voice_enabled: false,
        stripe_enabled: false,
        self_hosted: true,
        manual_review_enabled: false
    },
    gif: { provider: "klipy" },
    sso: { enabled: false, enforced: false, display_name: null, redirect_uri: "" },
    limits: { version: 2, traitDefinitions: [], rules: [], defaultsHash: "lite-mode-fallback" },
    push: { public_vapid_key: null },
    app_public: { sentry_dsn: "" },
    oauth2: {
        authorization_endpoint: "http://localhost:49319/oauth2/authorize",
        token_endpoint: "http://localhost:49319/oauth2/token",
        userinfo_endpoint: "http://localhost:49319/oauth2/userinfo",
        scopes_supported: ['identify', 'guilds', 'guilds.join', 'messages.read', 'messages.write', 'voice'],
    },
    federation: { enabled: true, version: 1 },
    public_key: { id: "https://localhost:49319/.well-known/fluxer#main-key", algorithm: 'x25519', public_key_base64: "dGVzdA==" }
};

try {
    const result = WellKnownFluxerResponse.parse(payload);
    console.log("Valid payload!");
} catch (e) {
    if (e instanceof z.ZodError) {
        console.error(JSON.stringify(e.errors, null, 2));
    } else {
        console.error("Unknown error:", e);
    }
}
