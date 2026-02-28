import { describe, it, expect } from 'vitest';
import { LoginRequest } from './packages/schema/src/domains/auth/AuthSchemas';

describe('LoginRequest', () => {
    it('should parse valid login', () => {
        const result = LoginRequest.safeParse({ email: "test@example.com", password: "password123!" });
        console.log("VALID LOGIN RESULT:", JSON.stringify(result, null, 2));
        expect(result.success).toBe(true);
    });

    it('should fail on missing email with invalid_type', () => {
        const result = LoginRequest.safeParse({ email: "", password: "password123!" });
        console.log("EMPTY EMAIL RESULT:", JSON.stringify(result, null, 2));
        expect(result.success).toBe(false);
    });
});
