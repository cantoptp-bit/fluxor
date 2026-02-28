import * as ftp from "basic-ftp";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));

async function cleanUpload() {
    const client = new ftp.Client();
    client.ftp.verbose = true;
    try {
        await client.access({
            host: "185.212.71.106",
            user: "u420534969",
            password: "Panera100$",
            secure: false
        });

        const subdomainDir = "/domains/auroraplayer.com/public_html/home";
        await client.cd(subdomainDir);

        console.log("Removing old index.html...");
        try {
            await client.remove("index.html");
        } catch (e) {
            console.log("No index.html to remove or error: " + e.message);
        }

        console.log("Uploading fresh index.html...");
        await client.uploadFrom(join(__dirname, "fluxer_app/dist/index.html"), "index.html");

        console.log("SUCCESS: index.html re-uploaded cleanly.");
    }
    catch (err) {
        console.error("FTP Error: ", err);
    }
    client.close();
}

cleanUpload();
