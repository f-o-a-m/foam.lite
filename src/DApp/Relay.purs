module DApp.Relay
  ( module DApp.Relay.Types
  , getRelayNonce
  , packSignature
  , mintRelayed
  , recoverRelayedMessageSignerWeb3
  , recoverRelayedTransferSignerWeb3
  ) where

import DApp.Relay.Types
import Prelude

import Contracts.RelayableNFT as RNFT
import DApp.Util (makeTxOpts)
import Data.ByteString as BS
import Data.Either (Either)
import Data.Symbol (SProxy(..))
import Network.Ethereum.Core.HexString (toByteString)
import Network.Ethereum.Core.Signatures (Signature(..))
import Network.Ethereum.Web3 (Address, ByteString, CallError, ChainCursor, HexString, TransactionOptions, UIntN, Web3)
import Network.Ethereum.Web3.Solidity.Sizes (S32)
import Network.Ethereum.Web3.Types (NoPay)
import Record as Record
import Type.Quotient (mkQuotient)

packSignature :: Signature -> ByteString
packSignature (Signature { r, s, v }) = packR <> packS <> packV
  where packR = toByteString r
        packS = toByteString s
        packV = BS.singleton $ mkQuotient v

getRelayNonce :: { rnftAddress :: Address, nonceOf :: Address, checkFrom :: Address, checkAt :: ChainCursor } -> Web3 (Either CallError (UIntN S32))
getRelayNonce { nonceOf, checkFrom, checkAt, rnftAddress } = do
  let txOpts = makeTxOpts { from: checkFrom, to: rnftAddress }
  RNFT.getCurrentRelayNonce txOpts checkAt { addr: nonceOf }

recoverRelayedMessageSignerWeb3 :: SignedRelayedMessage -> TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
recoverRelayedMessageSignerWeb3 (SignedRelayedMessage s) txOpts cc = 
  let packedSig = packSignature s.signature
      withPackedSig = Record.set (SProxy :: SProxy "signature") packedSig s
   in 
      RNFT.recoverRelayedMessageSigner txOpts cc { signature: packSignature s.signature, feeAmount: s.feeAmount, nonce: s.nonce, tokenURI: s.tokenURI }

recoverRelayedTransferSignerWeb3 :: SignedRelayedTransfer -> TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
recoverRelayedTransferSignerWeb3 (SignedRelayedTransfer s) txOpts cc = 
  let packedSig = packSignature s.signature
      withPackedSig = Record.set (SProxy :: SProxy "signature") packedSig s
   in
      RNFT.recoverRelayedTransferSigner txOpts cc withPackedSig

mintRelayed :: SignedRelayedMessage -> TransactionOptions NoPay -> Web3 HexString
mintRelayed (SignedRelayedMessage s) txOpts = do
  RNFT.mintRelayed txOpts { signature: packSignature s.signature, feeAmount: s.feeAmount, nonce: s.nonce, tokenURI: s.tokenURI }
  