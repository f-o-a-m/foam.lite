pragma solidity ^0.6.0;

// SPDX-License-Identifier: ISC

import "@openzeppelin/contracts/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/Counters.sol";

library RelayingUtils {
    using ECDSA for ECDSA;

    // This is similar to the OpenZeppelin Counters.Counter, but backed by a uint32
    // it also never decrements
    struct NonceCounter {
        uint32 _value;
    }

    function current(NonceCounter storage nonceCounter) internal view returns (uint32) {
        return nonceCounter._value;
    }

    function increment(NonceCounter storage nonceCounter) internal {
        // Much like with OZ's Counter, we don't need to overflow-check when adding 1
        nonceCounter._value += 1;
    }

    // Wrap a map address => counter to capture semantics of manipulating nonces
    struct NonceMap {
        mapping (address => NonceCounter) _nonces;
    }

    // Get the current (expected) nonce for address `a`
    function current(NonceMap storage nm, address a) internal view returns (uint32) {
        return current(nm._nonces[a]);
    }

    // Increment the expected nonce for address `a`
    function increment(NonceMap storage nm, address a) internal {
        increment(nm._nonces[a]);
    }

    struct RelayedMessage {
        uint32 nonce;
        uint256 feeAmount;
        bytes tokenData;
    }

    function messageHash(RelayedMessage memory message) internal pure returns (bytes32) {
        bytes memory packedMessage = abi.encode(message.nonce, message.feeAmount, message.tokenData);
        return keccak256(packedMessage);
    }

    function signingHash(RelayedMessage memory message) internal pure returns (bytes32) {
        return ECDSA.toEthSignedMessageHash(messageHash(message));
    }

    function recoverSigner(RelayedMessage memory message, bytes memory signature) internal pure returns (address) {
        return ECDSA.recover(signingHash(message), signature);
    }

    struct RelayedTransfer {
        uint32 nonce;
        uint256 feeAmount;
        uint256 tokenID;
        address destination;
    }

    function messageHash(RelayedTransfer memory transfer) internal pure returns (bytes32) {
        bytes memory packedMessage = abi.encode(transfer.nonce, transfer.feeAmount, transfer.tokenID, transfer.destination);
        return keccak256(packedMessage);
    }

    function signingHash(RelayedTransfer memory transfer) internal pure returns (bytes32) {
        return ECDSA.toEthSignedMessageHash(messageHash(transfer));
    }

    function recoverSigner(RelayedTransfer memory transfer, bytes memory signature) internal pure returns (address) {
        return ECDSA.recover(signingHash(transfer), signature);
    }
}