pragma solidity ^0.6.0;

// SPDX-License-Identifier: ISC

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC721/ERC721Burnable.sol";
import "@openzeppelin/contracts/utils/Counters.sol";
import "@openzeppelin/contracts/utils/EnumerableMap.sol";
import "./RelayingUtils.sol";

contract RelayableNFT is ERC721Burnable {
    using Counters for Counters.Counter;
    using EnumerableMap for EnumerableMap.UintToAddressMap;
    using RelayingUtils for RelayingUtils.RelayedMessage;
    using RelayingUtils for RelayingUtils.RelayedTransfer;
    using RelayingUtils for RelayingUtils.NonceMap;

    // Tracks the latest used token ID
    Counters.Counter private _tokenIDs;

    // Tracks the last used nonce for relay-minting
    RelayingUtils.NonceMap private _relayNonces;

    // Mapping from token ID to the actor that was responsible for its minting
    EnumerableMap.UintToAddressMap private _tokenMinters;

    // Stores the metadatas associated with minted tokens.
    mapping (uint256 => bytes) private _tokenDatas;

    // An ERC20 for paying fees with when relaying.
    IERC20 private _fungibleToken;

    event MintedByRelay(address minter, address relayer, uint tokenID);
    event TransferredByRelay(address owner, address destination, address relayer, uint tokenID);

    constructor(IERC20 fungibleToken) public ERC721("Relayable NFT", "RNFT") {
        require(address(fungibleToken) != address(0), "RelayableNFT: must have a FungibleToken associated with it");
        _fungibleToken = fungibleToken;
    }

    function _mintRelayableNFT(address owner, address minter, bytes memory tokenData) internal returns (uint256) {
        _tokenIDs.increment();

        uint256 newTokenID = _tokenIDs.current();
        _mint(owner, newTokenID);
        _setTokenMinter(newTokenID, minter);
        _setTokenData(newTokenID, tokenData);

        return newTokenID;
    }

    function mint(bytes memory tokenData) public returns (uint256) {
        return _mintRelayableNFT(msg.sender, msg.sender, tokenData);
    }

    function mintFor(address owner, bytes memory tokenData) public returns (uint256) {
        return _mintRelayableNFT(owner, msg.sender, tokenData);
    }

    function recoverRelayedMessageSigner(bytes memory signature, uint32 nonce, uint256 feeAmount, bytes memory tokenData) public pure returns (address) {
        // wrap the params into a relayedMessage
        RelayingUtils.RelayedMessage memory relayedMessage = RelayingUtils.RelayedMessage(
            { nonce: nonce, feeAmount: feeAmount, tokenData: tokenData }
        );
        return relayedMessage.recoverSigner(signature);
    }

    function recoverRelayedTransferSigner(bytes memory signature, uint32 nonce, uint256 feeAmount, uint256 tokenID, address destination) public pure returns (address) {
        // wrap the params into a relayedMessage
        RelayingUtils.RelayedTransfer memory relayedTransfer = RelayingUtils.RelayedTransfer(
            { nonce: nonce, feeAmount: feeAmount, tokenID: tokenID, destination: destination }
        );
        return relayedTransfer.recoverSigner(signature);
    }

    function relayedMessageSigningHash(uint32 nonce, uint256 feeAmount, bytes memory tokenData) public pure returns (bytes32) {
        // wrap the params into a relayedMessage
        RelayingUtils.RelayedMessage memory relayedMessage = RelayingUtils.RelayedMessage(
            { nonce: nonce, feeAmount: feeAmount, tokenData: tokenData }
        );
        return relayedMessage.signingHash();
    }

    function relayedTransferSigningHash(uint32 nonce, uint256 feeAmount, uint tokenID, address destination) public pure returns (bytes32) {
        // wrap the params into a relayedMessage
        RelayingUtils.RelayedTransfer memory relayedTransfer = RelayingUtils.RelayedTransfer(
            { nonce: nonce, feeAmount: feeAmount, tokenID: tokenID, destination: destination }
        );
        return relayedTransfer.signingHash();
    }

    function mintRelayed(bytes memory signature, uint32 nonce, uint256 feeAmount, bytes memory tokenData) public returns (uint256) {
        // wrap the params into a relayedMessage
        RelayingUtils.RelayedMessage memory relayedMessage = RelayingUtils.RelayedMessage(
            { nonce: nonce, feeAmount: feeAmount, tokenData: tokenData }
        );

        // Get the originator of the message from its signature
        address owner = relayedMessage.recoverSigner(signature);

        // we don't need to check that owner != address(0), as that will cause the transfer to always fail

        // The relayer is whoever is submitting the transaction
        address relayer = msg.sender;

        // Make sure the relayer isn't trying to replay a relay-mint to steal fees
        require(_relayNonces.current(owner) == relayedMessage.nonce,
            "RelayableNFT: The message contained an unexpected nonce");

        // Make sure this messsage can't be replayed
        _relayNonces.increment(owner);

        // Make sure the relayer gets paid
        require(_fungibleToken.transferFrom(owner, relayer, relayedMessage.feeAmount),
            "RelayableNFT: Could not transfer fee from message sender to relayer");

        // mint the token
        uint256 newTokenID = _mintRelayableNFT(owner, relayer, relayedMessage.tokenData);

        // emit the event
        emit MintedByRelay(owner, relayer, newTokenID);

        return newTokenID;
    }

    function transferRelayed(bytes memory signature, uint32 nonce, uint256 feeAmount, uint tokenID, address destination) public {
        // wrap the params into a relayedMessage
        RelayingUtils.RelayedTransfer memory relayedTransfer = RelayingUtils.RelayedTransfer(
            { nonce: nonce, feeAmount: feeAmount, tokenID: tokenID, destination: destination }
        );

        // Get the originator of the message from its signature
        address owner = relayedTransfer.recoverSigner(signature);

        // we don't need to check that owner != address(0), as that will cause the transfer to always fail

        // The relayer is whoever is submitting the transaction
        address relayer = msg.sender;

        // Make sure the relayer isn't trying to replay a relay-mint to steal fees
        require(_relayNonces.current(owner) == relayedTransfer.nonce,
            "RelayableNFT: The message contained an unexpected nonce");

        // Make sure this messsage can't be replayed
        _relayNonces.increment(owner);

        // Make sure the relayer gets paid
        require(_fungibleToken.transferFrom(owner, relayer, relayedTransfer.feeAmount),
            "RelayableNFT: Could not transfer fee from message sender to relayer");

        // openzeppelin requires you to explicitly use burn(uint256) instead of transferring to the zero address when burning
        // but we also need to sidestep isApprovedOrOwner because the owner may not have had a chance to approve the relayer to transfer
        // but we have the owner's signature
        require(_isApprovedOrOwner(owner, tokenID),
            "RelayableNFT: Relay transfer for a token thats not owned by the original signer");
        if (destination == address(0)) {
            _burn(tokenID);
        } else {
            _transfer(owner, destination, tokenID);
        }

        // emit the event
        emit TransferredByRelay(owner, destination, relayer, tokenID);
    }

    /**
     * @dev returns the next expected relay nonce
     */
    function getCurrentRelayNonce(address addr) public view returns (uint32) {
        return _relayNonces.current(addr);
    }

    /**
     * @dev Sets `_tokenMinter` as the recorded minter of tokenID
     */
    function _setTokenMinter(uint256 tokenID, address _tokenMinter) internal virtual {
        _tokenMinters.set(tokenID, _tokenMinter);
    }

    /**
     * @dev sets `_tokenData` as the tokenData of `tokenID`
     *
     * Requirements:
     *
     * - `tokenId` must exist
     */
    function _setTokenData(uint256 tokenId, bytes memory _tokenData) internal {
        require(_exists(tokenId), "RelayableNFT: Data set of nonexistent token");
        _tokenDatas[tokenId] = _tokenData;
    }

    function _burn(uint256 tokenId) internal virtual override {
        super._burn(tokenId);
        if (_tokenDatas[tokenId].length != 0) {
            delete _tokenDatas[tokenId];
        }
    }
}
