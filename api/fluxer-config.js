/**
 * Vercel serverless: returns backend URLs from FLUXER_PUBLIC_DOMAIN.
 * Used by the frontend when deployed on Vercel so the app can reach your backend
 * (e.g. ngrok) without baking the URL at build time.
 * Set FLUXER_PUBLIC_DOMAIN in Vercel env (e.g. your-ngrok.ngrok-free.app).
 */
export const config = { runtime: 'edge' };

export default function handler(_req) {
	const domain = process.env.FLUXER_PUBLIC_DOMAIN?.trim();
	if (!domain) {
		return new Response(
			JSON.stringify({ error: 'FLUXER_PUBLIC_DOMAIN not set' }),
			{ status: 200, headers: { 'Content-Type': 'application/json' } }
		);
	}
	const scheme = 'https';
	const api = `${scheme}://${domain}/api`;
	const gateway = `wss://${domain}/gateway`;
	return new Response(
		JSON.stringify({ base_domain: domain, api, gateway }),
		{ status: 200, headers: { 'Content-Type': 'application/json' } }
	);
}
