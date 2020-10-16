module Routes.ChainInfo where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ask)
import Data.Argonaut (class EncodeJson, encodeJson)
import Nodetrout (HTTPError)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.Signatures (Address)
import Type.Trout (type (:=), type (:/), Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)
import Types (AppM)

newtype ChainInfo =
  ChainInfo { id :: BigNumber
            , fungibleToken :: Address
            , relayableNFT  :: Address
            }

instance encodeJsonChainInfo :: EncodeJson ChainInfo where
  encodeJson (ChainInfo r) = encodeJson r

type ChainInfoRoute = "chain" := "chain" :/ Resource (Get ChainInfo JSON)

chainInfoRoute :: { "GET" :: ExceptT HTTPError AppM ChainInfo }
chainInfoRoute = 
  { "GET": do
    env <- ask
    pure $ ChainInfo { id: env.chainID, fungibleToken: env.addresses.fungibleToken, relayableNFT: env.addresses.relayableNFT }
  }