#ifndef FOAM_LITE_H__
#define FOAM_LITE_H__

#include "Blockchain.h"
#include "trezor-crypto/bignum.h"

#define MAX_MINT_DATA_SIZE 150
#define MAX_FEE_BITS 128

#define MAX_SIGNED_MESSAGE_LENGTH 255

typedef struct signed_message_t
{
	union {
		uint8_t raw[MAX_SIGNED_MESSAGE_LENGTH];
		struct {
			recoverable_sig_t sig;
			union {
				uint8_t body[MAX_SIGNED_MESSAGE_LENGTH - sizeof(recoverable_sig_t)];
				union {
					struct {
						uint8_t mintNonce[4];
						uint8_t mintFeeAmount[16];
						uint8_t mintDataLength;
						uint8_t mintData[MAX_SIGNED_MESSAGE_LENGTH - sizeof(recoverable_sig_t) - sizeof(uint32_t) - 16 - 1];
					};
					struct {
						uint8_t transferNonce[4];
						uint8_t transferFeeAmount[16];
						uint8_t transferTokenID[4];
						uint8_t transferDestination[20];
						uint8_t __PADDING[MAX_SIGNED_MESSAGE_LENGTH - sizeof(recoverable_sig_t) - (2*sizeof(uint32_t)) - 16 - 20];
					};
				} __attribute__((packed));
			} __attribute__((packed));
		} __attribute__((packed));
	} __attribute__((packed));
	size_t totalLength;
} signed_message_t;

class Bignum128 {
public:
	Bignum128(void);
	Bignum128(uint32_t);
	Bignum128(uint64_t);

	bignum256 get(void);
	bignum256* get_inner(void);

	void write_be(uint8_t* dest);
	static bool parse_hex_var_len(const char * hex, size_t len, Bignum128 *out);
private:
	bignum256 inner;
};

class FoamLiteMint {
public:
	uint32_t getNonce(void);
	Bignum128 getFeeAmount(void);
	Bignum128* ptrFeeAmount(void);
	bool getTokenData(uint8_t* dest, size_t length);
	size_t getTokenDataLength(void);

	void setNonce(uint32_t newNonce);
	void setFeeAmount(Bignum128 newFeeAmount);
	bool setTokenData(const uint8_t *data, size_t length);

	keccak256_t signingHash(void);
	blockchain_error_code_t sign(Blockchain* bc, signed_message_t *dest);
private:
	uint32_t nonce;
	Bignum128 feeAmount;
	uint8_t data[150];
	uint8_t dataLength;
};

class FoamLiteTransfer {
public:
	uint32_t getNonce(void);
	Bignum128 getFeeAmount(void);
	Bignum128* ptrFeeAmount(void);
	uint32_t getTokenID(void);
	eth_address_t getDestinationAddress(void);

	void setNonce(uint32_t newNonce);
	void setFeeAmount(Bignum128 newFeeAmount);
	void setTokenID(uint32_t newTokenID);
	void setDestinationAddress(eth_address_t addr);

	keccak256_t signingHash(void);
	blockchain_error_code_t sign(Blockchain* bc, signed_message_t *dest);
private:
	uint32_t nonce;
	Bignum128 feeAmount;
	uint32_t tokenID;
	eth_address_t destinationAddress;
};

#endif /* FOAM_LITE_H__ */
