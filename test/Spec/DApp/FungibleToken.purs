module Spec.DApp.FungibleToken (fungibleTokenSpec) where
  
import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Contracts.FungibleToken as FT
import DApp.Deploy.ContractConfig (DeployResults)
import DApp.Util (makeTxOpts)
import Data.Array as Array
import Data.Traversable (for, for_)
import Effect.Aff (joinFiber)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Core.BigNumber (embed, divide)
import Network.Ethereum.Web3 (ChainCursor(..), TransactionReceipt(..), TransactionStatus(..), UIntN, forkWeb3, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Spec.DApp.Common (SpecConfig)
import Spec.Helpers (expectRight, expectRight', expectWeb3', forceUIntN)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
  
fungibleTokenSpec :: SpecConfig DeployResults -> Spec Unit
fungibleTokenSpec { provider, primaryAccount, secondaryAccounts, fungibleToken, relayableNFT } = describe "FungibleToken" do
  let txOpts = makeTxOpts { from: primaryAccount, to: fungibleToken.deployAddress }
      totalSupply = fungibleToken.deployArgs.initialSupply
      (evenlyDistributedSupply :: UIntN S256) = forceUIntN $ (unUIntN totalSupply) `divide` (embed $ 1 + Array.length secondaryAccounts)
  it "has deployed with the entire balance owned by the primary account" do
    bal <- liftAff $ expectWeb3' "getting fungible token balance" provider $ FT.balanceOf txOpts Latest { account: primaryAccount }
    bal `shouldEqual` totalSupply

  it "can distribute the balance across the secondary accounts" do
    pars <- for secondaryAccounts $ \secondaryAccount -> forkWeb3 provider $ do
      txHash <- FT.transfer txOpts { recipient: secondaryAccount, amount: evenlyDistributedSupply }
      TransactionReceipt txr <- pollTransactionReceipt txHash provider
      txr.status `shouldEqual` Succeeded
      bal <- expectRight' =<< FT.balanceOf txOpts Latest { account: secondaryAccount }
      bal `shouldSatisfy` (_ >= evenlyDistributedSupply)
    for_ pars $ \par -> joinFiber par >>= expectRight
      
  it "can approve all accounts to spend with the RelayableNFT contract" do
    let allAccounts = Array.cons primaryAccount secondaryAccounts
    pars <- for allAccounts $ \account -> forkWeb3 provider do
      let approveTxOpts = makeTxOpts { from: account, to: fungibleToken.deployAddress }
      bal <- expectRight' =<< FT.balanceOf approveTxOpts Latest { account }
      log Debug $ "bal of " <> show account <> " is: " <> show bal
      bal `shouldSatisfy` (_ >= evenlyDistributedSupply)
      txHashSecondary <- FT.approve approveTxOpts { amount: bal, spender: relayableNFT.deployAddress }
      TransactionReceipt secondaryReceipt <- pollTransactionReceipt txHashSecondary provider
      secondaryReceipt.status `shouldEqual` Succeeded
    for_ pars $ \par -> joinFiber par >>= expectRight



      
    