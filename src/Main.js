var spawn = require('child_process').spawn;
var path = require('path');
var TelegramBot = require('node-telegram-bot-api');

exports._connect = function (token, eff) {
  return function () {
    eff(new TelegramBot(token, {polling: true}))();
  };
}

exports._sendMessage = function(bot, result) {
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

exports.addMessagesListener = function (bot, eff) {
  return function () {
    bot.onText(/^get$/i, function (msg, match) {
      var fromId = msg.from.id;
      eff({
        origin: 'request',
        id: fromId
      })();
      console.log('got request from', fromId);
    });
  };
}
