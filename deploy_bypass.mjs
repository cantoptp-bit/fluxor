import * as ftp from "basic-ftp";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));

async function deployBypass() {
    const client = new ftp.Client();
    client.ftp.verbose = true;
    try {
        console.log("Connecting to FTP...");
        await client.access({
            host: "185.212.71.106",
            user: "u420534969",
            password: "Panera100$",
            secure: false
        });

        console.log("Uploading files to /domains/auroraplayer.com/public_html/home...");
        await client.cd("/domains/auroraplayer.com/public_html/home");

        // Upload index.html and .htaccess
        await client.uploadFrom(join(__dirname, "fluxer_app/dist/index.html"), "index.html");
        await client.uploadFrom(join(__dirname, ".htaccess"), ".htaccess");

        // Upload the new assets folder
        console.log("Uploading fluxer-assets folder...");
        await client.ensureDir("fluxer-assets");
        await client.clearWorkingDir(); // Careful with this, but it's an empty dir now
        await client.uploadFromDir(join(__dirname, "fluxer_app/dist/fluxer-assets"));

        console.log("Deployment complete!");
    } catch (err) {
        console.error("Deployment failed:", err);
    } finally {
        client.close();
    }
}

deployBypass();
