module DApp.Util where

import Prelude

import Contracts.FungibleToken as FT
import Control.Monad.Error.Class (throwError)
import Data.Functor.Tagged (Tagged)
import Data.Generic.Rep (class Generic, Constructor)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Lens ((^.), (?~), (.~))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Exception (error)
import Network.Ethereum.Core.HexString (fromByteString, mkHexString, toByteString)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.RLP as RLP
import Network.Ethereum.Core.Signatures (Address, ChainId(..), PrivateKey, Signature(..), addChainIdOffset, privateToAddress, signMessage)
import Network.Ethereum.Web3 (BigNumber, Ether, HexString, TransactionOptions(..), UIntN, Value, Web3, _data, _from, _gas, _gasPrice, _nonce, _to, _value, convert, defaultTransactionOptions, embed, mkDataField, toMinorUnit, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3 as BS
import Network.Ethereum.Web3.Api (eth_estimateGas, eth_call, eth_gasPrice, eth_getTransactionCount, net_version)
import Network.Ethereum.Web3.Solidity (class GenericABIEncode, class RecordFieldsIso)
import Network.Ethereum.Web3.Solidity.Sizes (S128, S256, S32, s256)
import Network.Ethereum.Web3.Types (ETHER, ChainCursor(..))
import Network.Ethereum.Web3.Types.TokenUnit (class TokenUnitSpec, MinorUnit, NoPay)
import Partial.Unsafe (unsafePartialBecause)
import Type.Proxy (Proxy(..))

makeTxOpts :: { from :: Address, to :: Address } -> TransactionOptions NoPay
makeTxOpts { from, to } = defaultTransactionOptions
  # _from ?~ from
  # _to ?~ to

makePayEthTx :: { from :: Address, to :: Address, value :: Value Ether } -> TransactionOptions MinorUnit
makePayEthTx { from, to, value } =
  TransactionOptions { from: Just from
                     , to: Just to
                     , value: Just $ convert value
                     , gas: Nothing
                     , gasPrice: Nothing
                     , data: Nothing
                     , nonce: Nothing
                     }

widenUIntN32 :: UIntN S32 -> UIntN S256
widenUIntN32 u32 = unsafePartialBecause "we're expanding an uint32 -> uint256" fromJust $ uIntNFromBigNumber s256 (unUIntN u32)

widenUIntN128 :: UIntN S128 -> UIntN S256
widenUIntN128 u128 = unsafePartialBecause "we're expanding an uint128 -> uint256" fromJust $ uIntNFromBigNumber s256 (unUIntN u128)

newtype RawTransaction =
  RawTransaction { to :: Maybe Address
                 , value :: Maybe BigNumber
                 , gas :: BigNumber
                 , gasPrice :: BigNumber
                 , data :: HexString
                 , nonce :: BigNumber
                 }

derive instance genericRawTransaction :: Generic RawTransaction _
derive instance eqRawTransaction :: Eq RawTransaction

instance showRawTransaction :: Show RawTransaction where
  show = genericShow

makeUnsignedTransactionMessage ::
     ChainId
  -> RawTransaction
  -> BS.ByteString
makeUnsignedTransactionMessage (ChainId chainId) rawTx@(RawTransaction tx) =
  RLP.rlpEncode $
    RLP.RLPArray [ if tx.nonce == embed 0 then RLP.RLPNull else RLP.RLPBigNumber tx.nonce
                 , RLP.RLPBigNumber tx.gasPrice
                 , RLP.RLPBigNumber tx.gas
                 , maybe RLP.RLPNull RLP.RLPAddress tx.to
                 , maybe RLP.RLPNull RLP.RLPBigNumber tx.value
                 , RLP.RLPHexString tx.data
                 , RLP.RLPInt chainId
                 , RLP.RLPInt 0
                 , RLP.RLPInt 0
                 ]

makeSignedTransactionMessage :: Signature -> RawTransaction -> BS.ByteString
makeSignedTransactionMessage (Signature sig) rawTx@(RawTransaction tx) =
  RLP.rlpEncode $
    RLP.RLPArray [ if tx.nonce == embed 0 then RLP.RLPNull else RLP.RLPBigNumber tx.nonce
                 , RLP.RLPBigNumber tx.gasPrice
                 , RLP.RLPBigNumber tx.gas
                 , maybe RLP.RLPNull RLP.RLPAddress tx.to
                 , maybe RLP.RLPNull RLP.RLPBigNumber tx.value
                 , RLP.RLPHexString tx.data
                 , RLP.RLPInt sig.v
                 , RLP.RLPByteString (toByteString sig.r)
                 , RLP.RLPByteString (toByteString sig.s)
                 ]

-- in case the above ever breaks: leaving this in here...
-- foreign import makeSignedRawTransaction :: PrivateKey -> String -> Foreign -> ByteString
-- const EthJSTx = require('ethereumjs-tx').Transaction;
-- const EthJSCommon = require('ethereumjs-common').default;

-- exports.makeSignedRawTransaction = function(privateKey) {
--   return function(chainID) {
--     return function(txObject) {
--       const chainData = EthJSCommon.forCustomChain('mainnet', { name: 'network-specified-from-chain-id', networkId: parseInt(chainID), chainId: parseInt(chainID) }, 'muirGlacier');
--       const ejsTx = new EthJSTx(txObject, { common: chainData });
--       ejsTx.sign(privateKey);
--       return ejsTx.serialize().toString("hex");
--     }
--   }
-- }

-- | Signs an ABI call/transaction, without filling in nonce, etc.
signABIFn :: forall selector a name args fields l tokenUnit
           . IsSymbol selector
          => Generic a (Constructor name args)
          => GenericABIEncode (Constructor name args)
          => RecordFieldsIso args fields l
          => TokenUnitSpec (tokenUnit ETHER)
          => Proxy (Tagged (SProxy selector) a)
          -> PrivateKey
          -> ChainId
          -> TransactionOptions tokenUnit
          -> Record fields
          -> HexString
signABIFn proxy pk chainID (TransactionOptions txOpts) args = fromByteString $ makeSignedTransactionMessage sig rawTx
  where dataField = mkDataField proxy args
        rawTx =
          RawTransaction
              { to: txOpts.to
              , value: toMinorUnit <$> txOpts.value
              , gas: fromMaybe (embed 0) txOpts.gas
              , gasPrice: fromMaybe (embed 0) txOpts.gasPrice
              , data: dataField
              , nonce: fromMaybe (embed 0) txOpts.nonce
              }
        hashedRawTx = keccak256 (makeUnsignedTransactionMessage chainID rawTx)
        sig = addChainIdOffset chainID $ signMessage pk hashedRawTx

signTransaction' :: forall tokenUnit
                  . TokenUnitSpec (tokenUnit ETHER)
                 => PrivateKey -> TransactionOptions tokenUnit -> Web3 HexString
signTransaction' pk txo@(TransactionOptions txOpts) = do
  gasPriceToUse <- maybe eth_gasPrice pure txOpts.gasPrice
  nonceToUse <- maybe (eth_getTransactionCount (privateToAddress pk) Latest) pure txOpts.nonce
  chainId <- maybe (throwError $ error "Couldn't parse the node's chain ID as an Int!") pure =<< (map ChainId <<< Int.fromString <$> net_version)
  let getEstimatedGas = do
        let filledFromTxOpts = txo # _from ?~ (privateToAddress pk)
                                   # _value .~ (convert <$> txOpts.value)
                                   # _gas .~ Nothing
                                   # _gasPrice ?~ gasPriceToUse
                                   # _nonce ?~ nonceToUse
        nodeEstimate <- eth_estimateGas filledFromTxOpts
        pure $ nodeEstimate * (embed 125) / (embed 100) -- supply 1.25x the estimate, for safety factor
  gasToUse <- maybe getEstimatedGas pure txOpts.gas
  let rawTx =
          RawTransaction
              { to: txOpts.to
              , value: toMinorUnit <$> txOpts.value
              , gas: gasToUse
              , gasPrice: gasPriceToUse
              , data: fromMaybe (unsafePartialBecause "we're making an empty HexString" fromJust $ mkHexString "") txOpts.data
              , nonce: nonceToUse
              }
      hashedRawTx = keccak256 (makeUnsignedTransactionMessage chainId rawTx)
      sig = addChainIdOffset chainId $ signMessage pk hashedRawTx
  pure $ fromByteString $ makeSignedTransactionMessage sig rawTx

-- | Signs an ABI call/transaction, filling in unspecified nonce, gasprice, etc. by querying the node
signABIFn' :: forall selector a name args fields l tokenUnit
            . IsSymbol selector
           => Generic a (Constructor name args)
           => GenericABIEncode (Constructor name args)
           => RecordFieldsIso args fields l
           => TokenUnitSpec (tokenUnit ETHER)
           => Proxy (Tagged (SProxy selector) a)
           -> PrivateKey
           -> TransactionOptions tokenUnit
           -> Record fields
           -> Web3 HexString
signABIFn' proxy pk txo args = signTransaction' pk (txo # _data ?~ (mkDataField proxy args))

estimateABIFn':: forall selector a name args fields l tokenUnit
            . IsSymbol selector
           => Generic a (Constructor name args)
           => GenericABIEncode (Constructor name args)
           => RecordFieldsIso args fields l
           => TokenUnitSpec (tokenUnit ETHER)
           => Proxy (Tagged (SProxy selector) a)
           -> Address
           -> TransactionOptions tokenUnit
           -> Record fields
           -> Web3 BigNumber
estimateABIFn' proxy addr txo args = eth_estimateGas txo'
  where txo' = txo # _from ?~ addr
                   # _data ?~ (mkDataField proxy args)
                   # _value .~ (convert <$> txo ^. _value)

callABIFn':: forall selector a name args fields l tokenUnit
            . IsSymbol selector
           => Generic a (Constructor name args)
           => GenericABIEncode (Constructor name args)
           => RecordFieldsIso args fields l
           => TokenUnitSpec (tokenUnit ETHER)
           => Proxy (Tagged (SProxy selector) a)
           -> Address
           -> TransactionOptions tokenUnit
           -> Record fields
           -> Web3 HexString
callABIFn' proxy addr txo args = eth_call txo' Latest
  where txo' = txo # _from ?~ addr
                   # _data ?~ (mkDataField proxy args)
                   # _value .~ (convert <$> txo ^. _value)

signApprovalTx :: PrivateKey -> ChainId -> TransactionOptions NoPay -> { amount :: UIntN S256, spender :: Address } -> HexString
signApprovalTx = signABIFn (Proxy :: Proxy FT.ApproveFn)

signApprovalTx' :: PrivateKey -> TransactionOptions NoPay -> { amount :: UIntN S256, spender :: Address } -> Web3 HexString
signApprovalTx' = signABIFn' (Proxy :: Proxy FT.ApproveFn)