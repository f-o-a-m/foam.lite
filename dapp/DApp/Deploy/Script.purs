module DApp.Deploy.Script (deploy, deployScript) where
  
import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (DeployConfig(..), DeployM, ContractConfig)
import Control.Monad.Reader (ask)
import DApp.Deploy.ContractConfig (Contract, DeployResults, fungibleTokenConfig, relayableNFTConfig)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Validation.Semigroup as V
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (class KnownSize, Address, DLProxy, HexString, UIntN, _from, _gas, _gasPrice, defaultTransactionOptions, mkAddress, mkHexString, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

vToEither :: forall err res. V.V err res -> Either err res
vToEither = V.unV Left Right

wrapDeployed :: forall a
              . { description :: String
                , deployHash :: String
                , deployAddress :: String
                , deployArgs :: Record a
                }
              -> DeployM { deployAddress :: Address, deployHash :: HexString, deployArgs :: Record a }
wrapDeployed rec = do
  log Info $ "Using " <> rec.description <> " at " <> show rec.deployAddress
  pure { deployHash: mkHexString' rec.deployHash, deployAddress: mkAddress' rec.deployAddress, deployArgs: rec.deployArgs }

mkAddress' :: String -> Address
mkAddress' addr = case mkAddress =<< mkHexString addr of
  Nothing -> unsafeCrashWith $ "Invalid Address: " <> addr
  Just addr' -> addr'

mkHexString' :: String -> HexString
mkHexString' hx = case mkHexString hx of
  Nothing -> unsafeCrashWith $ "Invalid HexString: " <> hx
  Just hx' -> hx'

mkUIntN' :: forall s. KnownSize s => DLProxy s -> String -> UIntN s
mkUIntN' proxy str = case uIntNFromBigNumber proxy =<< parseBigNumber decimal str of
  Nothing -> unsafeCrashWith $ "Invalid UIntN: " <> str
  Just bn' -> bn'

deployScript :: DeployM (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig { networkID, primaryAccount, provider }) <- ask
  log Info $ "Deploying from account " <> show primaryAccount
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "5000000"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
                                         # _gasPrice ?~ (unsafePartial fromJust $ parseBigNumber decimal "60000000000")

      deployContract' :: forall args. ContractConfig args -> DeployM (Contract args)
      deployContract' = deployContract txOpts
      initialSupply = mkUIntN' s256 "1000000000000000000000000000"
  fungibleToken <- 
    if networkID == 1
    then wrapDeployed { description: "Mainnet FOAM Token for FungibleToken"
                      , deployHash: "0x6cadd5e8960f526803f23a72a9232492a028502a81aa71fb21beb6907d345313"
                      , deployAddress: "0x4946fcea7c692606e8908002e55a582af44ac121"
                      , deployArgs: { initialSupply }
                      }
    else if networkID == 4
    then wrapDeployed { description: "Rinkeby FOAM Token for FungibleToken"
                      , deployHash: "0x37a7f0318f8b6689a4667768a49050d4878b38323838ef8f88f1dc8173295597"
                      , deployAddress: "0xf450fab8717f7910fc508d2224cad0bf3321517a"
                      , deployArgs: { initialSupply }
                      }
    else deployContract' $ fungibleTokenConfig { initialSupply }
  relayableNFT <- deployContract' $ relayableNFTConfig { fungibleToken: fungibleToken.deployAddress }
  pure $ { fungibleToken, relayableNFT }

-- for chanterelle compatibility
deploy :: DeployM Unit
deploy = void deployScript