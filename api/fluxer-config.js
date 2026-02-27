/**
 * Vercel serverless: returns backend config from FLUXER_PUBLIC_DOMAIN at runtime.
 * So after you run scripts/set_vercel_backend_from_ngrok.ps1 you can refresh the
 * Vercel site and the app will get the current backend URL without redeploying.
 */
export default function handler(_req, res) {
	const domain = process.env.FLUXER_PUBLIC_DOMAIN?.trim();
	if (!domain) {
		res.status(200).json({ base_domain: null, api: null, gateway: null });
		return;
	}
	const base = domain.replace(/^https?:\/\//, '').split('/')[0];
	if (!base) {
		res.status(200).json({ base_domain: null, api: null, gateway: null });
		return;
	}
	res.setHeader('Cache-Control', 'no-store, max-age=0');
	res.status(200).json({
		base_domain: base,
		api: `https://${base}/api`,
		gateway: `wss://${base}/gateway`,
	});
}
