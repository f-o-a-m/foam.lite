"use strict";

let _baseURL;

function _getBaseURL() {
  if (!_baseURL) {
    const baseURL = process.env.URL;
    if (baseURL && typeof(baseURL) === 'string') {
      _baseURL = baseURL.replace(/\/$/, "");
    } else {
      _baseURL = "";
    }
  }

  return _baseURL;
}

exports.baseURL = _getBaseURL();

exports.getFallbackHTTPProviderURL = function(nothing) {
  return function(just) {
    try {
      const fallbackURL = process.env.FALLBACK_HTTP_PROVIDER;
      if (fallbackURL && fallbackURL != "") {
        return just(fallbackURL);
      }
    } catch (e) {
      console.log("getFallbackHTTPProvider caught", e);
      return nothing;
    }
  }
}

exports.getRelayedProvider = function(nothing) {
  const provider = window._relayed_provider;
  console.log("getRelayedProvider", provider);
  return function(just) {
    if (provider) {
      return just(provider);
    } else {
      return nothing;
    }
  }
}

exports.getRelayedContracts = function(nothing) {
  const contracts = window._relayed_contracts;
  console.log("getRelayedContracts", contracts);
  return function(just) {
    if (contracts) {
      return just(contracts);
    } else {
      return nothing;
    }
  }
}