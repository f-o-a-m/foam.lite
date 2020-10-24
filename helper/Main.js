"use strict";
// Chanterelle uses console.log, which prints to stdout
// We'd like to redirect it to stderr, without changing Chanterelle.
// So we have this hack...
const realConsoleLog = console.log;
console.log = console.error;

exports.realConsoleLog = function (s) {
  return function () {
    realConsoleLog(s);
    return {};
  };
};