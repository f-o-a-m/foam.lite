module DApp.Relay
  ( module DApp.Relay.Types
  , getRelayNonce
  , mintRelayed
  , transferRelayed
  , recoverRelayedMessageSignerWeb3
  , recoverRelayedTransferSignerWeb3
  ) where

import DApp.Relay.Types

import Contracts.RelayableNFT as RNFT
import DApp.Util (makeTxOpts, widenUIntN128, widenUIntN32)
import Data.Either (Either)
import Network.Ethereum.Web3 (Address, CallError, ChainCursor, HexString, TransactionOptions, UIntN, Web3)
import Network.Ethereum.Web3.Solidity.Sizes (S32)
import Network.Ethereum.Web3.Types (NoPay)

getRelayNonce :: { rnftAddress :: Address, nonceOf :: Address, checkFrom :: Address, checkAt :: ChainCursor } -> Web3 (Either CallError (UIntN S32))
getRelayNonce { nonceOf, checkFrom, checkAt, rnftAddress } = do
  let txOpts = makeTxOpts { from: checkFrom, to: rnftAddress }
  RNFT.getCurrentRelayNonce txOpts checkAt { addr: nonceOf }

recoverRelayedMessageSignerWeb3 :: SignedRelayedMessage -> TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
recoverRelayedMessageSignerWeb3 (SignedRelayedMessage s) txOpts cc = 
  RNFT.recoverRelayedMessageSigner txOpts cc { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenData: s.tokenData }

recoverRelayedTransferSignerWeb3 :: SignedRelayedTransfer -> TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
recoverRelayedTransferSignerWeb3 (SignedRelayedTransfer s) txOpts cc = 
  RNFT.recoverRelayedTransferSigner txOpts cc { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenID: (widenUIntN32 s.tokenID), destination: s.destination }

mintRelayed :: SignedRelayedMessage -> TransactionOptions NoPay -> Web3 HexString
mintRelayed (SignedRelayedMessage s) txOpts = do
  RNFT.mintRelayed txOpts { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenData: s.tokenData }
  
transferRelayed :: SignedRelayedTransfer -> TransactionOptions NoPay -> Web3 HexString
transferRelayed (SignedRelayedTransfer s) txOpts = do
  RNFT.transferRelayed txOpts { signature: packSignature s.signature, feeAmount: (widenUIntN128 s.feeAmount), nonce: s.nonce, tokenID: (widenUIntN32 s.tokenID), destination: s.destination }