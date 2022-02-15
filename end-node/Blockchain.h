#ifndef BLOCKCHAIN_H__
#define BLOCKCHAIN_H__

#include "ltsp.h"
#include "secp256k1.h"
#include "secp256k1_recovery.h"
#include "secp256k1_preallocated.h"

#include "RNGInterface.h"

#define USE_KECCAK 1
#include "trezor-crypto/sha3.h"


#define SECP256K1_PREALLOCATED_CONTEXT_FLAGS (SECP256K1_CONTEXT_VERIFY | SECP256K1_CONTEXT_SIGN)
#define SECP256K1_PREALLOCATED_CONTEXT_SIZE (184) // 184b with CONTEXT_VERIFY and CONTEXT_SIGN, aligned to a nice 32-byte boundary

#define PRIVATE_KEY_SIZE (32)
#define PUBLIC_KEY_UNCOMPRESSED_SIZE (64)
#define ETH_ADDRESS_SIZE (20)
#define SIGNATURE_SIZE (65)
#define SIGNATURE_DIGEST_SIZE (SHA3_256_DIGEST_LENGTH)

typedef struct eth_address_t
{
	uint8_t addr[ETH_ADDRESS_SIZE];
} __attribute__((packed)) eth_address_t;

typedef struct keccak256_t
{
	uint8_t hash[SHA3_256_DIGEST_LENGTH];
} __attribute__((packed)) keccak256_t;

typedef union recoverable_sig_t
{
	uint8_t raw[65];

	struct {
		union {
			uint8_t rs[64];
			struct {
				uint8_t r[32];
				uint8_t s[32];
			} __attribute__((packed));
		} __attribute__((packed));
		uint8_t v;
	} __attribute__((packed));
} __attribute__((packed)) recoverable_sig_t;


typedef enum blockchain_error_code_t
{
	BLOCKCHAIN_ERROR_CODE_NULL_PTR             = 0,
	BLOCKCHAIN_ERROR_CODE_INVALID_PRIVATE_KEY  = 1,
	BLOCKCHAIN_ERROR_CODE_UNPARSABLE_SIGNATURE = 2,
	BLOCKCHAIN_ERROR_CODE_ECRECOVER_FAILED     = 3,
	BLOCKCHAIN_ERROR_CODE_NO_ERROR             = 0xFF,
} blockchain_error_code_t;

#define SECP_RES this->secp256k1_last_result
#define SECP_CALL(fun, ...) \
	SECP_RES = secp256k1_##fun(this->secp256k1_ctx, ##__VA_ARGS__)

class Blockchain
{
  public:
    /**
     * Blockchain subsystem constructor.
     * Uses the default, global preallocated secp256k1 context...
     */
    Blockchain();
    Blockchain(secp256k1_context* ctx);
    ~Blockchain(void);

    /**
     * Gets the currently loaded private key
     */
    uint8_t* get_private_key(void);
    blockchain_error_code_t set_private_key(uint8_t* buf, size_t len);
    void generate_private_key(void);

    /**
     * Gets the uncompressed public key corresponding to the currently loaded private key
     * It does NOT include the 0x04 prefix signifying that the pubkey is uncompressed
     */
    uint8_t* get_uncompressed_public_key(void);

    /**
	 * Gets the 20-byte ethereum address corresponding to the currently loaded private key.
	 */
    eth_address_t get_eth_address(void);

    static size_t get_preallocated_context_size(void);

    /**
     * Calculate an ethereum address from a public key
     * \param uncompressed_pubkey    an uncompressed public key WITHOUT the 0x04 "uncompressed" prefix
     */
    static eth_address_t calculate_eth_address(const uint8_t uncompressed_pubkey[PUBLIC_KEY_UNCOMPRESSED_SIZE]);

    /**
	 * Calculate an ethereum address from a public key into an existing address buffer
	 * \param uncompressed_pubkey    an uncompressed public key WITHOUT the 0x04 "uncompressed" prefix
	 * \param destination            the eth_address_t to store the ethereum address in
	 */
	static void calculate_eth_address_in_place(const uint8_t uncompressed_pubkey[PUBLIC_KEY_UNCOMPRESSED_SIZE], eth_address_t *destination);

    /**
     * Attempts to recover the public key of a signed message
     * \param sig_digest         the digest that was signed with the private key
     * \param signature          the signature to recover the pubkey from
     * \param serialized_pubkey  65 byte array to output the serialized pubkey key into (includes the 0x04 UNCOMPRESSED prefix)
     */
    blockchain_error_code_t ec_recover(const uint8_t sig_digest[SIGNATURE_DIGEST_SIZE], const uint8_t signature[SIGNATURE_SIZE], uint8_t serialized_pubkey[PUBLIC_KEY_UNCOMPRESSED_SIZE]);

    /**
	 * Attempts to recover the public key of a signed message, then converts it to an ethereum address
	 * \param sig_digest         the digest that was signed with the private key
	 * \param signature          the signature to recover the pubkey from
	 * \param eth_addr           eth_address_t to output the recovered eth address into
	 */
    blockchain_error_code_t eth_addr_recover(const uint8_t sig_digest[SIGNATURE_DIGEST_SIZE], const uint8_t signature[SIGNATURE_SIZE], eth_address_t *dest);

    /**
	 * Attempts to sign a digest of a message
	 * \param sig_digest    the digest that was signed with the private key
	 * \param out           recoverable_sig_t to put the final signature into
	 */
    blockchain_error_code_t sign_digest(const uint8_t sig_digest[SIGNATURE_DIGEST_SIZE],  recoverable_sig_t* out);

  private:
    secp256k1_context *secp256k1_ctx;
    int secp256k1_last_result;
    uint8_t private_key[PRIVATE_KEY_SIZE];
    uint8_t public_key_uncompressed[PUBLIC_KEY_UNCOMPRESSED_SIZE + 1];
    eth_address_t eth_address;
};

#endif /* BLOCKCHAIN_H__ */
