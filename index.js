var spawn = require('child_process').spawn;
var path = require('path');
var Cycle = require('@cycle/core');
var Rx = require('rx');
var TelegramBot = require('node-telegram-bot-api');

var config = require('./config');
var token = config.token;
var torscraperPath = config.torscraperPath;
var master = config.master;

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

  Rx.Observable.merge(
    requests$,
    Rx.Observable.interval(1000 * 60 * 30).startWith(null).map(function () {})
  ).subscribe(function (id) {
    var source = id ? id : 'timer';
    var output = '';
    var torscraper = spawn('node', ['index.js'], {cwd: torscraperPath});
    console.log('processing request from', source);
    torscraper.stdout.on('data', function (data) {
      output += data;
    });
    torscraper.on('close', function () {
      console.log('close', output);
      if (!id && output.indexOf('nothing new to download') !== -1) {
        console.log('timer found nothing');
      } else {
        output$.onNext({
          id: id || master,
          output: output
        });
      }
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
