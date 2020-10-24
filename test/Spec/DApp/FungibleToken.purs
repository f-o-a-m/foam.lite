module Spec.DApp.FungibleToken (fungibleTokenSpec) where
  
import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Contracts.FungibleToken as FT
import DApp.Deploy.ContractConfig (DeployResults)
import DApp.Util (makePayEthTx, makeTxOpts, signApprovalTx')
import Data.Array as Array
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Traversable (for, for_)
import Effect.Aff (joinFiber)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Core.BigNumber (embed)
import Network.Ethereum.Web3 (ChainCursor(..), Ether, TransactionReceipt(..), TransactionStatus(..), UIntN, Value, Wei, _from, convert, forkWeb3, mkValue, runWeb3, unUIntN)
import Network.Ethereum.Web3.Api (eth_getBalance, eth_sendRawTransaction, eth_sendTransaction)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Spec.DApp.Common (SpecConfig)
import Spec.Helpers (expectRight, expectRight', expectWeb3', forceUIntN)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
  
fungibleTokenSpec :: SpecConfig DeployResults -> Spec Unit
fungibleTokenSpec { provider, primaryAccount, secondaryAccounts, fungibleToken, relayableNFT, nonWeb3Account, secondNonWeb3Account } = describe "FungibleToken" do
  let txOpts = makeTxOpts { from: primaryAccount, to: fungibleToken.deployAddress }
      totalSupply = fungibleToken.deployArgs.initialSupply
      -- we do 3 + number of secondaryAccounts -- one for primary and two for our non-web3 accounts
      (evenlyDistributedSupply :: UIntN S256) = forceUIntN $ (unUIntN totalSupply) / (embed $ 3 + Array.length secondaryAccounts)
  it "has deployed with the entire balance owned by the primary account" do
    bal <- liftAff $ expectWeb3' "getting fungible token balance" provider $ FT.balanceOf txOpts Latest { account: primaryAccount }
    bal `shouldEqual` totalSupply

  it "can distribute the balance across the secondary accounts" do
    pars <- for (secondaryAccounts <> [nonWeb3Account.address, secondNonWeb3Account.address]) $ \secondaryAccount -> forkWeb3 provider $ do
      txHash <- FT.transfer txOpts { recipient: secondaryAccount, amount: evenlyDistributedSupply }
      TransactionReceipt txr <- pollTransactionReceipt txHash provider
      txr.status `shouldEqual` Succeeded
      bal <- expectRight' =<< FT.balanceOf txOpts Latest { account: secondaryAccount }
      bal `shouldSatisfy` (_ >= evenlyDistributedSupply)
    for_ pars $ \par -> joinFiber par >>= expectRight

  it "can approve all Web3 accounts to spend with the RelayableNFT contract" do
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

  it "can prepare the non-web3 accounts to be tested" $ do
    let prepareNonWeb3Account acc = runWeb3 provider $ do
          let (ethToSend :: Value Ether) = mkValue (embed 3)
              (weiToSend :: Value Wei) = convert ethToSend
          txhETH <- eth_sendTransaction $ makePayEthTx { from: primaryAccount, to: acc.address, value: ethToSend }
          TransactionReceipt txrETH <- pollTransactionReceipt txhETH provider
          txrETH.status `shouldEqual` Succeeded
          balETH <- eth_getBalance acc.address Latest
          -- theres no way to get the bignumber out of a Value, so this should suffice
          (show balETH) `shouldEqual` (show weiToSend)
          approveTx <- signApprovalTx' acc.prv (txOpts # _from .~ Nothing) { spender: relayableNFT.deployAddress, amount: evenlyDistributedSupply }
          txhApprove <- eth_sendRawTransaction approveTx
          TransactionReceipt txrApprove <- pollTransactionReceipt txhApprove provider
          txrApprove.status `shouldEqual` Succeeded
    expectRight =<< prepareNonWeb3Account nonWeb3Account
    expectRight =<< prepareNonWeb3Account secondNonWeb3Account
