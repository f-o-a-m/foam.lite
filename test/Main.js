const crypto = require('crypto');
const secp256k1 = require('secp256k1');

exports.generatePrivateKey = function (_) {
  return function (pure) {
    var prv;
    do {
      prv = crypto.randomBytes(32)
    } while (!secp256k1.privateKeyVerify(prv));
    return pure(prv.toString("hex"));
  }
}