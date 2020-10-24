"use strict";
exports.ledgerHttpProvider = function (providerUrl) {
  return function (ledgerOptions) {
    return function () {
      var provider;
      try {
        require("regenerator-runtime/runtime");
        var LedgerWalletProvider = require("truffle-ledger-provider");
        provider = new LedgerWalletProvider(ledgerOptions, providerUrl);
      } catch (e) {
        throw new Error("Please run `npm install truffle-ledger-provider` to use Ledger functionality. It is omitted by default to keep the build compatible across platforms.\n" + e)
      }

      if (ledgerOptions.networkId && ledgerOptions.networkId > 0xff) {
        console.warn("Some Ledger models have a bug where Network IDs >= 255 will seem to work, but still generate invalid signatures.\nWhile we will continue, you can expect your transactions to fail...")
      }
      return provider;
    };
  };
};

exports.stopLedgerProvider = function(engine) {
  return function () {
    engine.stop();
  };
};