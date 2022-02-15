#include "Blockchain.h"

static uint8_t secp256k1_preallocated_ctx_mem[SECP256K1_PREALLOCATED_CONTEXT_SIZE] __attribute((aligned(16))) /*__attribute__((section (".ccmram"))) */;


Blockchain::Blockchain()
{
  if (sizeof(secp256k1_preallocated_ctx_mem) < Blockchain::get_preallocated_context_size()) {
    for(;;) { }
  }
  this->secp256k1_ctx = secp256k1_context_preallocated_create(&secp256k1_preallocated_ctx_mem, SECP256K1_CONTEXT_VERIFY | SECP256K1_CONTEXT_SIGN);
}

Blockchain::Blockchain(secp256k1_context* ctx): secp256k1_ctx(ctx)
{
}

Blockchain::~Blockchain(void)
{
  // zero out private key on destruction
  memset(this->private_key, 0, PRIVATE_KEY_SIZE);
}

uint8_t* Blockchain::get_private_key(void) {
  return this->private_key;
}


uint8_t* Blockchain::get_uncompressed_public_key(void) {
  return &this->public_key_uncompressed[1];
}

eth_address_t Blockchain::get_eth_address(void) {
  return this->eth_address;
}

blockchain_error_code_t Blockchain::set_private_key(uint8_t* buf, size_t len) {
	if (buf == NULL || len != PRIVATE_KEY_SIZE) {
		return BLOCKCHAIN_ERROR_CODE_INVALID_PRIVATE_KEY;
	}

	SECP_CALL(ec_seckey_verify, buf);
	if(!SECP_RES) {
		return BLOCKCHAIN_ERROR_CODE_INVALID_PRIVATE_KEY;
	} else {
		secp256k1_pubkey pubkey;
		size_t ser_len = sizeof(this->public_key_uncompressed);
		memset(this->private_key, 0, PRIVATE_KEY_SIZE);
		memset(this->public_key_uncompressed, 0, sizeof(this->public_key_uncompressed));
		memcpy(this->private_key, buf, len);
		SECP_CALL(ec_pubkey_create, &pubkey, &this->private_key[0]);
		if(!SECP_RES) {
			return BLOCKCHAIN_ERROR_CODE_INVALID_PRIVATE_KEY;
		}
		SECP_CALL(ec_pubkey_serialize, this->public_key_uncompressed, &ser_len, &pubkey, SECP256K1_EC_UNCOMPRESSED);

		Blockchain::calculate_eth_address_in_place(&this->public_key_uncompressed[1], &this->eth_address);
 		return BLOCKCHAIN_ERROR_CODE_NO_ERROR;
	}
}

void Blockchain::generate_private_key(void)
{
	uint8_t random_secret[PRIVATE_KEY_SIZE];
	blockchain_error_code_t res;
	do
	{
		for (int i = 0; i < PRIVATE_KEY_SIZE; i += 4) {
			uint32_t random = rng.get();
			random_secret[i    ] = (random      ) & 0xFF;
			random_secret[i + 1] = (random >> 8 ) & 0xFF;
			random_secret[i + 2] = (random >> 16) & 0xFF;
			random_secret[i + 3] = (random >> 24) & 0xFF;
		}
		res = this->set_private_key(random_secret, sizeof(random_secret));
	} while (res != BLOCKCHAIN_ERROR_CODE_NO_ERROR);
}

size_t Blockchain::get_preallocated_context_size(void)
{
  return secp256k1_context_preallocated_size(SECP256K1_PREALLOCATED_CONTEXT_FLAGS);
}

eth_address_t Blockchain::calculate_eth_address(const uint8_t pubkey[PUBLIC_KEY_UNCOMPRESSED_SIZE])
{
	eth_address_t ret = { .addr = { 0 } };
	Blockchain::calculate_eth_address_in_place(pubkey, &ret);
	return ret;
}

void Blockchain::calculate_eth_address_in_place(const uint8_t pubkey[PUBLIC_KEY_UNCOMPRESSED_SIZE], eth_address_t *destination)
{
	uint8_t pubkey_digest[SHA3_256_DIGEST_LENGTH] = { 0 };
	keccak_256(pubkey, PUBLIC_KEY_UNCOMPRESSED_SIZE, pubkey_digest);
	// -21 to start at the _index_ of the 20th-to-last byte of the digest
	memcpy(&(destination->addr), &pubkey_digest[SHA3_256_DIGEST_LENGTH-ETH_ADDRESS_SIZE], ETH_ADDRESS_SIZE);
}

blockchain_error_code_t Blockchain::ec_recover(const uint8_t sig_digest[SIGNATURE_DIGEST_SIZE], const uint8_t signature[SIGNATURE_SIZE], uint8_t serialized_pubkey[PUBLIC_KEY_UNCOMPRESSED_SIZE + 1])
{
	secp256k1_ecdsa_recoverable_signature rsig;
	secp256k1_pubkey rpub;
	size_t ser_len = PUBLIC_KEY_UNCOMPRESSED_SIZE + 1;
	const uint8_t *rs = &signature[0];
	uint8_t v = signature[64];
	if (v == 27 || v == 28) {
		v -= 27;
	} else if (v != 0 && v != 1) {
		return BLOCKCHAIN_ERROR_CODE_UNPARSABLE_SIGNATURE;
	}
	SECP_CALL(ecdsa_recoverable_signature_parse_compact, &rsig, rs, v);
	if (this->secp256k1_last_result != 1) {
		return BLOCKCHAIN_ERROR_CODE_UNPARSABLE_SIGNATURE;
	}
	SECP_CALL(ecdsa_recover, &rpub, &rsig, sig_digest);
	if (this->secp256k1_last_result != 1) {
		return BLOCKCHAIN_ERROR_CODE_ECRECOVER_FAILED;
	}
	SECP_CALL(ec_pubkey_serialize, serialized_pubkey, &ser_len, &rpub, SECP256K1_EC_UNCOMPRESSED);
	return BLOCKCHAIN_ERROR_CODE_NO_ERROR;

}

blockchain_error_code_t Blockchain::eth_addr_recover(const uint8_t sig_digest[SIGNATURE_DIGEST_SIZE], const uint8_t signature[SIGNATURE_SIZE], eth_address_t* dest)
{
	uint8_t serialized_pubkey[PUBLIC_KEY_UNCOMPRESSED_SIZE + 1] = { 0 };
	blockchain_error_code_t err = this->ec_recover(sig_digest, signature, (uint8_t*) &serialized_pubkey);
	if (err == BLOCKCHAIN_ERROR_CODE_NO_ERROR) {
		Blockchain::calculate_eth_address_in_place(&serialized_pubkey[1], dest);
	}

	return err;
}

blockchain_error_code_t Blockchain::sign_digest(const uint8_t sig_digest[SIGNATURE_DIGEST_SIZE], recoverable_sig_t *out)
{
	secp256k1_ecdsa_recoverable_signature rsig;
	SECP_CALL(ecdsa_sign_recoverable, &rsig, &sig_digest[0], this->private_key, NULL, NULL);
	if (this->secp256k1_last_result != 1) {
		return BLOCKCHAIN_ERROR_CODE_INVALID_PRIVATE_KEY;
	}
	int recid;
	SECP_CALL(ecdsa_recoverable_signature_serialize_compact, out->rs, &recid, &rsig);
	out->v = (recid + 27);
	return BLOCKCHAIN_ERROR_CODE_NO_ERROR;
}
