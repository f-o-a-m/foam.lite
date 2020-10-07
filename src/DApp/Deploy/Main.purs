module DApp.Deploy.Main (main) where
  
import Prelude

import Chanterelle.Deploy (deploy)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Process (lookupEnv)

import DApp.Deploy.Script (deployScript)

main :: Effect Unit
main = do
  nodeUrl <- fromMaybe "http://localhost:8545" <$> lookupEnv "NODE_URL"
  abiPrefix <- fromMaybe "" <$> lookupEnv "DEPLOY_ABI_PREFIX"
  launchAff_ $ deploy nodeUrl 60 deployScript