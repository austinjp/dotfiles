#!/usr/bin/env node

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function waterstones() {
  while(true) {
    const r = await fetch("http://10.255.76.251:8002/index.php?zone=waterstones", { "credentials": "omit", "headers": { "User-Agent": "Mozilla/5.0 (X11; Linux x86_64; rv:106.0) Gecko/20100101 Firefox/106.0", "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8", "Accept-Language": "en-GB,en;q=0.5", "Content-Type": "application/x-www-form-urlencoded", "Upgrade-Insecure-Requests": "1" }, "referrer": "http://192.168.1.1/", "body": "auth_user=user1&auth_pass=user1&redirurl=http%3A%2F%2Fwifi.waterstones.com&zone=waterstones&accept=Start+Browsing", "method": "POST", "mode": "cors" });
    console.log("Fetch response: ", r.statusText);
    console.log("Sleeping for 10 seconds...");
    await sleep(10000);
  }
}

waterstones();
