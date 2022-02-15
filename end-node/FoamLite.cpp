#include "FoamLite.h"
#include "util.h"

Bignum128::Bignum128(void)
{
	bn_zero(&inner);
}

Bignum128::Bignum128(uint32_t u32)
{
	bn_read_uint32(u32, &inner);
}

Bignum128::Bignum128(uint64_t u64)
{
	bn_read_uint64(u64, &inner);
}

bool Bignum128::parse_hex_var_len(const char *hex, size_t len, Bignum128 *out)
{
	bignum256* bn = &out->inner;
	bn_zero(bn);
	if (len == 0 || len > 32 || len % 2 != 0) {
		return false;
	}

	uint8_t buf[32] = {0};
	int bufIxStart = 32 - (len / 2);

	for (size_t i = 0; i < len; i++) {
		uint8_t nibble = hex_to_nibble(hex[i]);
		if (nibble == 0 && hex[i] != '0') {
			return false;
		}
		int bufIx = bufIxStart + (i / 2);
		if (i % 2 == 0) {
			buf[bufIx] = (nibble << 4);
		} else {
			buf[bufIx] |= nibble & 0x0F;
		}
	}

	bn_read_be(buf, bn);
	return true;
}

void Bignum128::write_be(uint8_t* dest)
{
	uint8_t tmp[32] = { 0 };
	bn_write_be(&this->inner, tmp);
	memcpy(dest, &tmp[16], 16);
}

bignum256 Bignum128::get(void)
{
	return inner;
}

bignum256* Bignum128::get_inner(void)
{
	return &(this->inner);
}

// ============================================


uint32_t FoamLiteMint::getNonce(void)
{
	return nonce;
}

Bignum128 FoamLiteMint::getFeeAmount(void)
{
	return feeAmount;
}

Bignum128* FoamLiteMint::ptrFeeAmount(void)
{
	return &feeAmount;
}

bool FoamLiteMint::getTokenData(uint8_t* dest, size_t length)
{
	if (length < dataLength) {
		return false;
	}
	memcpy(dest, &data, dataLength);
	return true;
}

size_t FoamLiteMint::getTokenDataLength(void)
{
	return dataLength;
}

void FoamLiteMint::setNonce(uint32_t newNonce)
{
	nonce = newNonce;
}

void FoamLiteMint::setFeeAmount(Bignum128 newFeeAmount)
{
	feeAmount = newFeeAmount;
}

bool FoamLiteMint::setTokenData(const uint8_t *newData, size_t length)
{
	if (length > MAX_MINT_DATA_SIZE) {
		return false;
	}

	memcpy(&data, newData, length);
	dataLength = length;
	return true;
}

keccak256_t FoamLiteMint::signingHash(void)
{
	uint8_t packing_buf[288] = { 0 };
	uint8_t personal_sign_buf[61] = ("\x19""Ethereum Signed Message:\n32\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
	keccak256_t res;

	// pack nonce (32bit -> 256 bit)
	packing_buf[28] = uint8_t((nonce & 0xFF000000) >> 24);
	packing_buf[29] = uint8_t((nonce & 0x00FF0000) >> 16);
	packing_buf[30] = uint8_t((nonce & 0x0000FF00) >> 8);
	packing_buf[31] = uint8_t((nonce & 0x000000FF));

	// write the fee amount
	bn_write_be(feeAmount.get_inner(), &packing_buf[32]);

	// string offset for the message is a constant 96, since we have 3x uint256 for nonce, fee, and (this) string length
	packing_buf[95] = 96;

	// our message length will never be > MAX_MINT_DATA_SIZE which is 150, so we can just take its length wholesale
	packing_buf[127] = uint8_t(dataLength);

	// finally, we can copy our message into a "left-aligned" string, into the remainder of packing_buf
	// we start at 128, since we have 32b for nonce as uint256, + 32b for feeAmount, 32b for messageLength,
	memcpy(&packing_buf[128], data, dataLength);

	// non-packed ABI encoding means we have to right-pad up to 256-bits (32 bytes), so whatever string we end up signing
	// will be treated as though it were right-padded with \0 to the nearest 32bytes
	size_t encodedDataLength = dataLength + (32 - (dataLength % 32));
	size_t packedDataLength = 128 + encodedDataLength;

	// keccak packing_buf into the end of personal_sign_buf (after the personal_sign prefix);
	keccak_256(packing_buf, packedDataLength, &personal_sign_buf[28]);

	// then sign personal_sign_buf into res!
	keccak_256((uint8_t*)personal_sign_buf, 60, &(res.hash[0]));


	return res;
}

blockchain_error_code_t FoamLiteMint::sign(Blockchain* blockchain, signed_message_t *dest)
{
	if (blockchain == NULL || dest == NULL) {
		return BLOCKCHAIN_ERROR_CODE_NULL_PTR;
	}

	blockchain_error_code_t bcerr;

	keccak256_t signingHash = this->signingHash();
	memset(dest, 0, sizeof(signed_message_t));

	bcerr = blockchain->sign_digest(signingHash.hash, &(dest->sig));
	if (bcerr != BLOCKCHAIN_ERROR_CODE_NO_ERROR) {
		return bcerr;
	}

	write_be(dest->mintNonce, nonce);
	feeAmount.write_be(dest->mintFeeAmount);
	dest->mintDataLength = uint8_t(dataLength);
	memcpy(dest->mintData, data, dataLength);

	dest->totalLength = 86 + dataLength; // 65 byte recoverable sig, 4 byte nonce, 16 byte fee, 1 byte mint data len, n byte minting data


	return BLOCKCHAIN_ERROR_CODE_NO_ERROR;
}

// ============================================


uint32_t FoamLiteTransfer::getNonce(void)
{
	return nonce;
}

Bignum128 FoamLiteTransfer::getFeeAmount(void)
{
	return feeAmount;
}

Bignum128* FoamLiteTransfer::ptrFeeAmount(void)
{
	return &feeAmount;
}

uint32_t FoamLiteTransfer::getTokenID(void)
{
	return tokenID;
}

eth_address_t FoamLiteTransfer::getDestinationAddress(void)
{
	return destinationAddress;
}

void FoamLiteTransfer::setNonce(uint32_t newNonce)
{
	nonce = newNonce;
}
void FoamLiteTransfer::setFeeAmount(Bignum128 newFeeAmount)
{
	feeAmount = newFeeAmount;
}
void FoamLiteTransfer::setTokenID(uint32_t newTokenID)
{
	tokenID = newTokenID;
}
void FoamLiteTransfer::setDestinationAddress(eth_address_t newDestination)
{
	destinationAddress = newDestination;
}

keccak256_t FoamLiteTransfer::signingHash(void)
{
	uint8_t packing_buf[128] = { 0 };
	uint8_t personal_sign_buf[61] = "\x19""Ethereum Signed Message:\n32\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
	keccak256_t res;

	// pack nonce (32bit -> 256 bit)
	packing_buf[28] = uint8_t((nonce & 0xFF000000) >> 24);
	packing_buf[29] = uint8_t((nonce & 0x00FF0000) >> 16);
	packing_buf[30] = uint8_t((nonce & 0x0000FF00) >> 8);
	packing_buf[31] = uint8_t((nonce & 0x000000FF));

	// write the fee amount
	bn_write_be(feeAmount.get_inner(), &packing_buf[32]);

	// pack the token ID
	packing_buf[92] = uint8_t((tokenID & 0xFF000000) >> 24);
	packing_buf[93] = uint8_t((tokenID & 0x00FF0000) >> 16);
	packing_buf[94] = uint8_t((tokenID & 0x0000FF00) >> 8);
	packing_buf[95] = uint8_t((tokenID & 0x000000FF));

	// pack the destination
	memcpy(&packing_buf[107], &(destinationAddress.addr), ETH_ADDRESS_SIZE);

	// keccak packing_buf into the end of personal_sign_buf;
	keccak_256(packing_buf, 128, &personal_sign_buf[28]);

	// then sign personal_sign_buf into res!
	keccak_256(&personal_sign_buf[0], 60, &res.hash[0]);


	return res;
}

blockchain_error_code_t FoamLiteTransfer::sign(Blockchain* blockchain, signed_message_t *dest)
{
	if (blockchain == NULL || dest == NULL) {
		return BLOCKCHAIN_ERROR_CODE_NULL_PTR;
	}

	blockchain_error_code_t bcerr;

	keccak256_t signingHash = this->signingHash();
	dest->totalLength = 0;
	memset(dest, 0, sizeof(signed_message_t));

	bcerr = blockchain->sign_digest(signingHash.hash, &(dest->sig));
	if (bcerr != BLOCKCHAIN_ERROR_CODE_NO_ERROR) {
		return bcerr;
	}

	write_be(dest->transferNonce, nonce);

	feeAmount.write_be(dest->transferFeeAmount);

	write_be(dest->transferTokenID, tokenID);

	memcpy(dest->transferDestination, destinationAddress.addr, ETH_ADDRESS_SIZE);
	dest->totalLength = 109; // 65 byte recoverable sig, 4 byte nonce, 16 byte fee, 4 byte tokenID, 20 byte destination

	return BLOCKCHAIN_ERROR_CODE_NO_ERROR;
}
