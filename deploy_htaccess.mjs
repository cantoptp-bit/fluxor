import * as ftp from "basic-ftp";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));

async function deployHtaccess() {
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

        console.log("Navigating to subdomain public_html...");
        await client.cd("/domains/auroraplayer.com/public_html/home");

        console.log("Uploading .htaccess...");
        await client.uploadFrom(join(__dirname, ".htaccess"), ".htaccess");

        console.log("Successfully uploaded .htaccess!");
    }
    catch (err) {
        console.error("FTP Error: ", err);
    }
    client.close();
}

deployHtaccess();
