var fs = require('fs');
var spawn = require('child_process').spawn;
var path = require('path');
var TelegramBot = require('node-telegram-bot-api');

exports.readTextFile = function (string, callback) {
  return function () {
    fs.readFile(string, 'utf8', function (err, data) {
      callback(data)();
    });
  };
}

exports.parseConfig = function (string) {
  var config = JSON.parse(string);

  if (!config.token) throw "missing {token} in config (telegram bot token)"
  if (!config.torscraperPath) throw "missing {torscraperPath} in config (path to torscraper)"
  if (!config.master) throw "missing {master} in config (numeric id of telegram master)"

  return config;
}

exports.runTorscraper = function (torscraperPath, request, callback) {
  return function () {
    var origin = request.origin;
    var id = request.id;
    var output = '';
    var torscraper = spawn('node', ['index.js'], {cwd: torscraperPath});
    console.log('processing request from', origin);
    torscraper.stdout.on('data', function (data) {
      output += data;
    });
    torscraper.on('close', function () {
      if (!id && output.indexOf('nothing new to download') !== -1) {
        callback({
          id: origin,
          output: '',
          origin: origin
        })();
      } else {
        callback({
          id: id,
          output: output,
          origin: origin
        })();
      }
    });
  };
}

exports.connect = function (token, callback) {
  return function () {
    callback(new TelegramBot(token, {polling: true}))();
    console.log('connected to Telegram');
  };
}

exports.sendMessage = function(bot, result) {
  return function () {
    var id = result.id;
    var output = result.output;
    var origin = result.origin;

    if (output.length > 0) {
      if (origin === 'timer' && output.indexOf('nothing new to download') !== -1) {
        console.log('timer found nothing');
      } else {
        console.log(output);
        bot.sendMessage(id, output);
      }
    }
  };
}

exports.addMessagesListener = function (bot, callback) {
  return function () {
    bot.onText(/^get$/i, function (msg, match) {
      var fromId = msg.from.id;
      callback({
        origin: 'request',
        id: fromId
      })();
      console.log('got request from', fromId);
    });
  };
}

exports.interval = function (time, id, callback) {
  return function () {
    var tick = function () {
      callback({
        origin: 'timer',
        id: id
      })();
    };
    tick();
    setInterval(tick, time);
  };
}
