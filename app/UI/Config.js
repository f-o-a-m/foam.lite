"use strict";

exports.providerURLImpl = function() {
  return process.env.PROVIDER_URL; // eslint-disable-line no-undef
};

exports.relayableNFTImpl = function() {
  return process.env.RELAYABLE_NFT; // eslint-disable-line no-undef
};