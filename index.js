const cp = require("child_process");

const TelegramBot = require("node-telegram-bot-api");

// token, torscraperPath, master
const config = require("./config");

function runScraper() {
  console.log("running scraper");
  try {
    const result = cp.spawnSync("node", ["index.js"], {
      cwd: config.torscraperPath,
    });
    const stdout = result.stdout.toString();

    console.log("stdout:", stdout);
    return stdout;
  } catch (e) {
    const err = `Error in runScraper: ${e.toString()}`;

    console.log(err);
    return err;
  }
}

function main() {
  const token = config.token;
  const bot = new TelegramBot(token, { polling: true });

  bot.onText(/^get$/i, (msg, match) => {
    console.log("get msg:", msg?.from?.id);
    if (msg?.from?.id !== config.master) {
      console.log("failed to match master");
      return;
    } else {
      console.log("message run");
      const result = runScraper();
      bot.sendMessage(config.master, result);
    }
  });

  bot.onText(/^disk$/i, (msg, match) => {
    console.log("disk msg:", msg?.from?.id);
    if (msg?.from?.id !== config.master) {
      console.log("failed to match master");
      return;
    } else {
      console.log("message run");
      const result = checkDiskStatus();
      bot.sendMessage(config.master, result);
    }
  });

  function runTimer() {
    console.log("timer run");
    const result = runScraper();
    if (result.trim() !== "nothing new to download") {
      bot.sendMessage(config.master, result);
    }
  }

  setInterval(runTimer, 30 * 60 * 1000);
  runTimer();

  console.log("running");
}

function checkDiskStatus() {
  cp.exec("df -h /", (error, stdout, stderr) => {
    if (error) {
      console.error(`Error: ${error.message}`);
      return;
    }
    if (stderr) {
      console.error(`Error: ${stderr}`);
      return;
    }

    const lines = stdout.trim().split("\n");
    const [, size, used, available, usedPercentage, mountPoint] =
      lines[1].split(/\s+/);

    return `Root Disk Status:
  Filesystem: ${lines[1].split(/\s+/)[0]}
  Total Size: ${size}
  Free Space: ${available}
  Used Space: ${used} (${usedPercentage})`;
  });
}

main();
