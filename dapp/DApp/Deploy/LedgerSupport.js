"use strict";
require("regenerator-runtime/runtime");
var LedgerWalletProvider = require("truffle-ledger-provider");
exports.ledgerHttpProvider = function (providerUrl) {
  return function (ledgerOptions) {
    return function () {
      var provider = new LedgerWalletProvider(ledgerOptions, providerUrl);
      return provider;
    };
  };
};

exports.stopLedgerProvider = function(engine) {
  return function () {
    engine.stop();
  };
};