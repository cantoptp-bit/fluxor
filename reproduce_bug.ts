
import { z } from 'zod';
import { LoginRequest } from './packages/schema/src/domains/auth/AuthSchemas';
import { fluxerZodErrorMap } from './packages/api/src/ZodErrorMap';

const testInput = {
    email: 'test@fluxer.app',
    password: 'testpassword'
};

async function test() {
    const result = await LoginRequest.safeParseAsync(testInput, { errorMap: fluxerZodErrorMap });
    if (!result.success) {
        console.log('Validation failed:');
        console.log(JSON.stringify(result.error.issues, null, 2));
    } else {
        console.log('Validation succeeded!');
    }
}

test();
