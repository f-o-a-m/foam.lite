module DApp.Util where
  
import Prelude

import Contracts.FungibleToken as FT
import Data.Functor.Tagged (Tagged)
import Data.Generic.Rep (class Generic, Constructor)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Symbol (class IsSymbol, SProxy)
import Foreign.Generic (class Encode)
import Network.Ethereum.Core.HexString (fromByteString, toByteString)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.RLP as RLP
import Network.Ethereum.Core.Signatures (Address, ChainId(..), PrivateKey, Signature(..), addChainIdOffset, signMessage)
import Network.Ethereum.Web3 (BigNumber, Ether, HexString, TransactionOptions(..), UIntN, Value, _from, _to, convert, defaultTransactionOptions, embed, mkDataField, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3 as BS
import Network.Ethereum.Web3.Solidity (class GenericABIEncode, class RecordFieldsIso)
import Network.Ethereum.Web3.Solidity.Sizes (S128, S256, S32, s256)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit, NoPay)
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

signABIFn :: forall selector a name args fields l payability
           . IsSymbol selector
          => Generic a (Constructor name args)
          => GenericABIEncode (Constructor name args)
          => RecordFieldsIso args fields l
          => Encode (TransactionOptions payability)
          => Proxy (Tagged (SProxy selector) a)
          -> PrivateKey
          -> ChainId
          -> TransactionOptions NoPay
          -> Record fields
          -> HexString
signABIFn proxy pk chainID (TransactionOptions txOpts) args = fromByteString $ makeSignedTransactionMessage sig rawTx
  where dataField = mkDataField proxy args
        rawTx =
          RawTransaction
              { to: txOpts.to
              , value: Nothing
              , gas: fromMaybe (embed 0) txOpts.gas
              , gasPrice: fromMaybe (embed 0) txOpts.gasPrice
              , data: dataField
              , nonce: fromMaybe (embed 0) txOpts.nonce
              }
        hashedRawTx = keccak256 (makeUnsignedTransactionMessage chainID rawTx)
        sig = addChainIdOffset chainID $ signMessage pk hashedRawTx

signApprovalTx :: PrivateKey -> ChainId -> TransactionOptions NoPay -> { amount :: UIntN S256, spender :: Address } -> HexString
signApprovalTx = signABIFn (Proxy :: Proxy FT.ApproveFn)