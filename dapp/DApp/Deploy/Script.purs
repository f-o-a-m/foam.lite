module DApp.Deploy.Script (deployScript) where
  
import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (DeployConfig(..), DeployM, ContractConfig)
import Control.Monad.Reader (ask)
import DApp.Deploy.ContractConfig (Contract, DeployResults, fungibleTokenConfig, relayableNFTConfig)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (_from, _gas, _gasPrice, defaultTransactionOptions, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafePartial)

deployScript :: DeployM (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig { primaryAccount, provider }) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "5000000"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
                                         # _gasPrice ?~ (unsafePartial fromJust $ parseBigNumber decimal "2000000000")
      deployContract' :: forall args. ContractConfig args -> DeployM (Contract args)
      deployContract' = deployContract txOpts
      initialSupply = unsafePartial fromJust $ uIntNFromBigNumber s256 =<< parseBigNumber decimal "10100000000000000000000000000000000"
  fungibleToken <- deployContract' $ fungibleTokenConfig { initialSupply }
  relayableNFT <- deployContract' $ relayableNFTConfig { fungibleToken: fungibleToken.deployAddress }
  pure $ { fungibleToken, relayableNFT }