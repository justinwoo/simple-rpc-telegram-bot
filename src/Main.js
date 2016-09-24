var spawn = require('child_process').spawn;
var path = require('path');
var TelegramBot = require('node-telegram-bot-api');

exports.connect = function (token) {
  return function () {
    return new TelegramBot(token, {polling: true});
  };
}

exports._sendMessage = function(bot, id, message) {
  return function () {
    bot.sendMessage(id, message);
  };
}

exports.addMessagesListener = function (bot, User, eff) {
  return function () {
    bot.onText(/^get$/i, function (msg, match) {
      var fromId = msg.from.id;
      eff({
        origin: User,
        id: fromId
      })();
      console.log('got request from', fromId);
    });
  };
}
