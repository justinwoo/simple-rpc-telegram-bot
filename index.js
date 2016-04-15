var spawn = require('child_process').spawn;
var path = require('path');
var Cycle = require('@cycle/core');
var Rx = require('rx');
var TelegramBot = require('node-telegram-bot-api');

var config = require('./config');
var token = config.token;
var torscraperPath = config.torscraperPath;

function botDriver(input$) {
  var requests$ = new Rx.ReplaySubject(1);
  var bot = new TelegramBot(token, {polling: true});

  function sendMessage(fromId, resp) {
    console.log('responding to user ', fromId);
    console.log(resp);
    bot.sendMessage(fromId, resp);
  }

  function getShows(msg) {
    var fromId = msg.from.id;
    console.log('processing getshows');
    requests$.onNext(fromId);
  }

  bot.onText(/^gs$/i, function (msg, match) {
    getShows(msg);
  });

  bot.onText(/^get$/i, function (msg, match) {
    getShows(msg);
  });

  input$.subscribe(function (input) {
    sendMessage(input.id, input.output);
  });

  console.log('boot sequence finished!!!');

  return requests$;
}

function torscraperDriver(requests$) {
  var output$ = new Rx.ReplaySubject(1);

  requests$.subscribe(function (id) {
    console.log('processing request from ', id);
    var output = '';
    var torscraper = spawn('node', ['index.js'], {cwd: torscraperPath});
    torscraper.stdout.on('data', function (data) {
      console.log('have data', data);
      output += data;
    });
    torscraper.on('close', function () {
      console.log('close', output);
      output$.onNext({
        id: id,
        output: output
      });
    });
  });

  return output$;
}

function main(sources) {
  return {
    bot: sources.torscraper,
    torscraper: sources.bot
  };
}

Cycle.run(main, {
  bot: botDriver,
  torscraper: torscraperDriver
});
