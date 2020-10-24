module Spec.DApp.Message where

import Prelude

import DApp.Message (DAppMessage, parseDAppMessage)
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Foldable (for_)
import Data.Tuple (fst)
import Effect.Class (class MonadEffect, liftEffect)
import Random.LCG (randomSeed)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (runGen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

messageSpec :: Spec Unit
messageSpec = describe "DApp.Message" do
  it "maintains Show/Parse identity" do
    for_ (enumFromTo 1 100 :: Array Int) $ \_ -> do
      msg <- mkMessage
      let encoded = show msg
          decoded = parseDAppMessage encoded
      decoded `shouldEqual` Right msg

mkMessage :: forall m. MonadEffect m => m DAppMessage
mkMessage = do
    newSeed <- liftEffect randomSeed
    pure $ fst $ runGen arbitrary $ { newSeed, size: 10 }