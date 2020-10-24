module Spec.Helpers where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.MonadZero (guard)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Aff, Error)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (error)
import Network.Ethereum.Web3 (class EventFilter, class KnownSize, Address, BigNumber, Change(..), EventAction(..), Filter, HexString, Provider, UIntN, Web3, embed, event, forkWeb3, mkAddress, mkHexString, runWeb3, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, DLProxy(..))
import Partial.Unsafe (unsafePartialBecause)

bnZero :: BigNumber
bnZero = embed 0

zeroAddress :: Address
zeroAddress = unsafePartialBecause "we can make the zero address manually" fromJust $ mkAddress =<< mkHexString "0x0000000000000000000000000000000000000000"

forceUIntN :: forall len. KnownSize len => BigNumber -> UIntN len
forceUIntN = unsafePartialBecause "we're knowingly forcing a BigNumber into a size UInt" fromJust <<< uIntNFromBigNumber (DLProxy :: DLProxy len)

embedUIntN :: forall len. KnownSize len => Int -> UIntN len
embedUIntN = unsafePartialBecause "we expect to be able to embed Int into any length of UIntN" fromJust <<< uIntNFromBigNumber (DLProxy :: DLProxy len) <<< embed

uintZero :: forall len. KnownSize len => UIntN len
uintZero = embedUIntN 0

expectWeb3 :: forall a. String -> Provider -> Web3 a -> Aff a
expectWeb3 msg provider action = do
  res <- runWeb3 provider action
  case res of
    Left err -> throwError $ error $ "Web3 " <> msg <> " failed: " <> show err
    Right a -> pure a

expectWeb3' :: forall l r. Show l => String -> Provider -> Web3 (Either l r) -> Aff r
expectWeb3' msg provider action = do
  res <- runWeb3 provider action
  case res of
    Left err -> throwError $ error $ "Web3 failed while performing " <> msg <> ": " <> show err
    Right a -> case a of
      Left err -> throwError $ error $ "Web3 failed while performing " <> msg <> ": " <> show err
      Right a' -> pure a'

expectRight :: forall l r m. Show l => MonadThrow Error m =>  Either l r -> m Unit
expectRight = void <<< expectRight'

expectRight' :: forall l r m. Show l => MonadThrow Error m =>  Either l r -> m r
expectRight' = case _ of
  Left e -> throwError $ error $ "Expected a `Right` but got (Left " <> show e <> ")"
  Right r -> pure r

expectRight'' :: forall l1 l2 r m. Show l1 => Show l2 => MonadThrow Error m => Either l1 (Either l2 r) -> m r
expectRight'' e = expectRight' =<< expectRight' e

awaitEvent :: forall m evt i ni b
            . MonadAff m
           => EventFilter evt
           => DecodeEvent i ni evt
           => Provider
           -> Filter evt
           -> (evt -> ReaderT Change Maybe b)
           -> m b
awaitEvent provider filter mapper = liftAff do
  avar <- AVar.empty
  _ <- forkWeb3 provider $ do
    event filter $ \(e :: evt) -> do
      change <- ask
      case runReaderT (mapper e) change of
        Nothing -> pure ContinueEvent
        Just a -> liftAff (AVar.put a avar $> TerminateEvent)
  AVar.read avar

filterForTransactionHash :: forall evt
                          . EventFilter evt
                         => HexString
                         -> (evt -> ReaderT Change Maybe evt)
filterForTransactionHash txh = \e -> do
  (Change c) <- ask
  guard $ c.transactionHash == txh
  pure e

resizeUIntN :: forall a b. KnownSize a => KnownSize b => UIntN a -> UIntN b
resizeUIntN = forceUIntN <<< unUIntN