import { describe, it } from 'vitest';
import { LoginRequest } from '../AuthSchemas';

describe('LoginRequest', () => {
    const testCases = [
        { name: "Trailing space", data: { email: "test@example.com ", password: "password123!" } },
        { name: "Uppercase email", data: { email: "Test@Example.com", password: "password123!" } },
        { name: "Plus in email", data: { email: "test+1@example.com", password: "password123!" } },
        { name: "No TLD", data: { email: "test@localhost", password: "password123!" } },
        { name: "IP address", data: { email: "test@127.0.0.1", password: "password123!" } },
        { name: "Password with space", data: { email: "test@example.com", password: " password123! " } },
        { name: "Short password", data: { email: "test@example.com", password: "short" } },
        { name: "Empty email", data: { email: "", password: "password123!" } },
        { name: "Null email", data: { email: null, password: "password123!" } }
    ];

    testCases.forEach(({ name, data }) => {
        it(`tests ${name}`, () => {
            const result = LoginRequest.safeParse(data);
            if (!result.success) {
                console.log(`Failed [${name}]:`);
                result.error.issues.forEach(issue => {
                    console.log(` - code: ${issue.code}, message: ${issue.message}, path: ${issue.path.join('.')}`);
                });
            } else {
                console.log(`Passed [${name}]`);
            }
        });
    });
});
