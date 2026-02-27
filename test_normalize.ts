
import { normalizeString } from './packages/schema/src/primitives/SchemaPrimitives';

const email = 'test@fluxer.app';
const normalizedEmail = normalizeString(email);
console.log(`Original: "${email}", Normalized: "${normalizedEmail}"`);

const password = 'testpassword';
const normalizedPassword = normalizeString(password);
console.log(`Original: "${password}", Normalized: "${normalizedPassword}"`);
