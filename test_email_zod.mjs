// @ts-check
import { z } from 'zod';

const MAX_STRING_PROCESSING_LENGTH = 10000;
const RTL_OVERRIDE_REGEX = /\u202E/g;
const FORM_FEED_REGEX = /\u000C/g;

function normalizeString(value) {
    return (
        value
            .replace(RTL_OVERRIDE_REGEX, '')
            .replace(FORM_FEED_REGEX, '')
            .replace(/[\x00-\x09\x0B\x0C\x0E-\x1F\x7F]/g, '')
            .trim()
    );
}

function withStringLengthRangeValidation(schema, minLength, maxLength, errorCode) {
    return schema.superRefine((value, ctx) => {
        if (value.length < minLength || value.length > maxLength) {
            const params = { min: minLength, max: maxLength };
            if (minLength === maxLength) {
                params.length = minLength;
            }
            ctx.addIssue({ code: 'custom', message: errorCode, params });
        }
    });
}

const EMAIL_LOCAL_REGEX = /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+$/;

const EmailType = z
    .string()
    .min(1, 'EMAIL_IS_REQUIRED')
    .transform(normalizeString)
    .pipe(
        withStringLengthRangeValidation(
            z.string().email('INVALID_EMAIL_FORMAT'),
            1,
            254,
            'EMAIL_LENGTH_INVALID',
        ),
    )
    .refine((value) => {
        const atIndex = value.indexOf('@');
        if (atIndex === -1) return false;
        const local = value.slice(0, atIndex);
        return EMAIL_LOCAL_REGEX.test(local);
    }, 'INVALID_EMAIL_LOCAL_PART');

console.log(JSON.stringify(EmailType.safeParse('cantoptp@gmail.com'), null, 2));
console.log(JSON.stringify(EmailType.safeParse('test'), null, 2));
