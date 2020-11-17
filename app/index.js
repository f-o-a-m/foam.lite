'use strict';
import detectEthereumProvider from '@metamask/detect-provider';

async function run() {
  console.log('loading stylesheet');
  require("./style.css");
  console.log('loaded stylesheet');

  const provider = await detectEthereumProvider();
  if (provider) {
    console.log("got a web3 provider, ostensibly:", provider);

    provider.on('chainChanged', (_chain) => window.location.reload());
    provider.on('chainIdChanged', (_chainId) => window.location.reload());
    provider.on('networkChanged', (_network) => window.location.reload());


    window._relayed_provider = provider;
  }

  window._relayed_contracts = {
    relayableNFT: require('../build/RelayableNFT.json'),
  };

  console.log('app starting from entrypoint');
  require('./Main.purs').main();

  if (module.hot) {
    module.hot.accept();
  }
}

run();

