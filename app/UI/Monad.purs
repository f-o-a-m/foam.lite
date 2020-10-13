module UI.Monad where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Network.Ethereum.Web3 (Address, Provider)


type Contracts = 
  { relayableNFT :: Address
  }

newtype AppEnv = AppEnv
  { web3Provider :: Provider
  , contracts :: Contracts
  }

newtype AppM a = AppM (ReaderT AppEnv Aff a) 

runAppM :: AppEnv -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadAsk AppEnv AppM