module DApp.Relay
  ( module DApp.Relay.Types
  , getRelayNonce
  , mintRelayed
  , transferRelayed
  , mintRelayed'
  , transferRelayed'
  , recoverRelayedMessageSignerWeb3
  , recoverRelayedTransferSignerWeb3
  , estimateMintRelayed
  , estimateTransferRelayed
  , callMintRelayed
  , callTransferRelayed
  ) where

import DApp.Relay.Types
import Prelude (($))

import Contracts.RelayableNFT as RNFT
import DApp.Util (estimateABIFn', callABIFn', makeTxOpts, signABIFn', widenUIntN128, widenUIntN32)
import Data.Either (Either)
import Network.Ethereum.Core.Signatures (PrivateKey)
import Network.Ethereum.Web3 (Address, BigNumber, CallError, ChainCursor, HexString, TransactionOptions, UIntN, Web3)
import Network.Ethereum.Web3.Solidity.Sizes (S32)
import Network.Ethereum.Web3.Types (NoPay)
import Type.Proxy (Proxy(..))

getRelayNonce :: { rnftAddress :: Address, nonceOf :: Address, checkFrom :: Address, checkAt :: ChainCursor } -> Web3 (Either CallError (UIntN S32))
getRelayNonce { nonceOf, checkFrom, checkAt, rnftAddress } =
  let txOpts = makeTxOpts { from: checkFrom, to: rnftAddress }
   in RNFT.getCurrentRelayNonce txOpts checkAt { addr: nonceOf }

recoverRelayedMessageSignerWeb3 :: SignedRelayedMessage -> TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
recoverRelayedMessageSignerWeb3 (SignedRelayedMessage s) txOpts cc =
  RNFT.recoverRelayedMessageSigner txOpts cc { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenData: s.tokenData }

recoverRelayedTransferSignerWeb3 :: SignedRelayedTransfer -> TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
recoverRelayedTransferSignerWeb3 (SignedRelayedTransfer s) txOpts cc =
  RNFT.recoverRelayedTransferSigner txOpts cc { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenID: (widenUIntN32 s.tokenID), destination: s.destination }

mintRelayed :: SignedRelayedMessage -> TransactionOptions NoPay -> Web3 HexString
mintRelayed (SignedRelayedMessage s) txOpts =
  RNFT.mintRelayed txOpts { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenData: s.tokenData }

mintRelayed' :: PrivateKey -> SignedRelayedMessage -> TransactionOptions NoPay -> Web3 HexString
mintRelayed' pk (SignedRelayedMessage s) txOpts =
  signABIFn' (Proxy :: Proxy RNFT.MintRelayedFn) pk txOpts $
    { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenData: s.tokenData }

transferRelayed :: SignedRelayedTransfer -> TransactionOptions NoPay -> Web3 HexString
transferRelayed (SignedRelayedTransfer s) txOpts =
  RNFT.transferRelayed txOpts { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenID: (widenUIntN32 s.tokenID), destination: s.destination }

transferRelayed' :: PrivateKey -> SignedRelayedTransfer -> TransactionOptions NoPay -> Web3 HexString
transferRelayed' pk (SignedRelayedTransfer s) txOpts =
  signABIFn' (Proxy :: Proxy RNFT.TransferRelayedFn) pk txOpts $
    { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenID: (widenUIntN32 s.tokenID), destination: s.destination }

estimateMintRelayed :: Address -> SignedRelayedMessage -> TransactionOptions NoPay -> Web3 BigNumber
estimateMintRelayed relayerAddress  (SignedRelayedMessage s) txOpts =
  estimateABIFn' (Proxy :: Proxy RNFT.MintRelayedFn) relayerAddress txOpts $
    { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenData: s.tokenData }

estimateTransferRelayed :: Address -> SignedRelayedTransfer -> TransactionOptions NoPay -> Web3 BigNumber
estimateTransferRelayed relayerAddress (SignedRelayedTransfer s) txOpts =
  estimateABIFn' (Proxy :: Proxy RNFT.TransferRelayedFn) relayerAddress txOpts $
    { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenID: (widenUIntN32 s.tokenID), destination: s.destination }

callMintRelayed :: Address -> SignedRelayedMessage -> TransactionOptions NoPay -> Web3 HexString
callMintRelayed relayerAddress  (SignedRelayedMessage s) txOpts =
  callABIFn' (Proxy :: Proxy RNFT.MintRelayedFn) relayerAddress txOpts $
    { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenData: s.tokenData }

callTransferRelayed :: Address -> SignedRelayedTransfer -> TransactionOptions NoPay -> Web3 HexString
callTransferRelayed relayerAddress (SignedRelayedTransfer s) txOpts =
  callABIFn' (Proxy :: Proxy RNFT.TransferRelayedFn) relayerAddress txOpts $
    { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenID: (widenUIntN32 s.tokenID), destination: s.destination }
