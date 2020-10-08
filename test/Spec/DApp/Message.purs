module Spec.DApp.Message where

import DApp.Message
import Prelude

import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Foldable (for_)
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
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
    let (Tuple res _) = runGen arbitrary $ { newSeed, size: 10 }
    pure res
          
  