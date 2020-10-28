'use strict';

console.log('app starting from entrypoint');
require('./Main.purs').main();

if (module.hot) {
  module.hot.accept();
}

