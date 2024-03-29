module Spec.DApp.RelayableNFT where

import Prelude

import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Contracts.RelayableNFT as RNFT
import Control.MonadZero (guard)
import DApp.Deploy.ContractConfig (DeployResults)
import DApp.Relay (UnsignedRelayedMessage(..), UnsignedRelayedTransfer(..), getRelayNonce, mintRelayed, mintRelayed', recoverRelayedMessageSignerWeb3, recoverRelayedTransferSignerWeb3, signRelayedMessage, signRelayedMessageWeb3, signRelayedTransfer, signRelayedTransferWeb3, transferRelayed, transferRelayed')
import DApp.Util (makeTxOpts)
import Data.Array ((!!))
import Data.ByteString (toUTF8) as BS
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Effect.Aff (forkAff, joinFiber)
import Network.Ethereum.Web3 (ChainCursor(..), TransactionReceipt(..), TransactionStatus(..), Web3Error(..), embed, eventFilter, runWeb3)
import Network.Ethereum.Web3.Api (eth_sendRawTransaction)
import Network.Ethereum.Web3.Types (RpcError(..))
import Partial.Unsafe (unsafePartial)
import Spec.DApp.Common (SpecConfig)
import Spec.Helpers (awaitEvent, expectRight', expectRight'', forceUIntN, resizeUIntN, zeroAddress)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldContain)
import Type.Proxy (Proxy(..))

relayableNFTSpec :: SpecConfig DeployResults -> Spec Unit
relayableNFTSpec { provider, primaryAccount, secondaryAccounts, fungibleToken, relayableNFT, accountPassword, nonWeb3Account, secondNonWeb3Account } = describe "RelayableNFT" do
  let relayFeeAmount = forceUIntN $ embed 100
      txOpts = makeTxOpts { from: primaryAccount, to: relayableNFT.deployAddress }
  it "can be minted like any NFT" do
    txh <- expectRight' =<< (runWeb3 provider $ RNFT.mint txOpts { tokenData: BS.toUTF8 "Regular mint!"})
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded

  it "can be transferred like any NFT" do
    let secondaryAccount = unsafePartial fromJust $ secondaryAccounts !! 2
    fEv <- forkAff $ awaitEvent provider (eventFilter (Proxy :: Proxy RNFT.Transfer) relayableNFT.deployAddress) (\(RNFT.Transfer ev) -> (guard $ ev.to == primaryAccount) $> ev.tokenId)
    txh <- expectRight' =<< (runWeb3 provider $ RNFT.mint txOpts { tokenData: BS.toUTF8 "Regular mint for transfer!"})
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded
    tokenId <- joinFiber fEv
    txh' <- expectRight' =<< (runWeb3 provider $ RNFT.transferFrom txOpts { from: primaryAccount, to: secondaryAccount, tokenId })
    TransactionReceipt txr' <- pollTransactionReceipt txh' provider
    txr'.status `shouldEqual` Succeeded
    ownerOfToken <- expectRight'' =<< (runWeb3 provider $ RNFT.ownerOf txOpts Latest { tokenId })
    ownerOfToken `shouldEqual` secondaryAccount

  it "can be burned" do
    let secondaryAccount = unsafePartial fromJust $ secondaryAccounts !! 2
    fEv <- forkAff $ awaitEvent provider (eventFilter (Proxy :: Proxy RNFT.Transfer) relayableNFT.deployAddress) (\(RNFT.Transfer ev) -> (guard $ ev.to == primaryAccount) $> ev.tokenId)
    txh <- expectRight' =<< (runWeb3 provider $ RNFT.mint txOpts { tokenData: BS.toUTF8 "Regular mint for burn!"})
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded
    tokenId <- joinFiber fEv
    txh' <- expectRight' =<< (runWeb3 provider $ RNFT.burn txOpts { tokenId })
    TransactionReceipt txr' <- pollTransactionReceipt txh' provider
    txr'.status `shouldEqual` Succeeded
    eOwnerOf <- (runWeb3 provider $ RNFT.ownerOf txOpts Latest { tokenId })
    -- depending on version of ethereum client, openzeppelin, etc., we might either get a revert with
    -- a helpful message, or the zero address
    case eOwnerOf of
      Left (Rpc (RpcError err)) -> err.message `shouldContain` "nonexistent token"
      other -> do
        ownerOfToken <- expectRight'' other
        ownerOfToken `shouldEqual` zeroAddress

  it "can be minted for someone else via mintFor" do
    let secondaryAccount = unsafePartial fromJust $ secondaryAccounts !! 0
    fEv <- forkAff $ awaitEvent provider (eventFilter (Proxy :: Proxy RNFT.Transfer) relayableNFT.deployAddress) (\(RNFT.Transfer ev) -> (guard $ ev.to == secondaryAccount) $> ev.tokenId)
    txh <- expectRight' =<< (runWeb3 provider $ RNFT.mintFor txOpts { owner: secondaryAccount, tokenData: BS.toUTF8 "mintFor mint" } )
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded
    tokenId <- joinFiber fEv
    ownerOfToken <- expectRight'' =<< (runWeb3 provider $ RNFT.ownerOf txOpts Latest { tokenId })
    ownerOfToken `shouldEqual` secondaryAccount

  it "can be minted for someone else via mintRelayed" do
    let secondaryAccount = unsafePartial fromJust $ secondaryAccounts !! 1
        password = accountPassword secondaryAccount
    fEv <- forkAff $ awaitEvent provider (eventFilter (Proxy :: Proxy RNFT.Transfer) relayableNFT.deployAddress) (\(RNFT.Transfer ev) -> (guard $ ev.to == secondaryAccount) $> ev.tokenId)
    txh <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress , nonceOf: secondaryAccount, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedMessage { nonce, feeAmount: relayFeeAmount, tokenData: BS.toUTF8 "relay mint!" }
      signedMessage <- signRelayedMessageWeb3 secondaryAccount password msg
      recoveredSigner <- expectRight' =<< recoverRelayedMessageSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` secondaryAccount
      mintRelayed signedMessage txOpts
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded
    tokenId <- joinFiber fEv
    ownerOfToken <- expectRight'' =<< (runWeb3 provider $ RNFT.ownerOf txOpts Latest { tokenId })
    ownerOfToken `shouldEqual` secondaryAccount

  it "can be minted for someone else via mintRelayed, with purescript-eth-core doing the signing" do
    fEv <- forkAff $ awaitEvent provider (eventFilter (Proxy :: Proxy RNFT.Transfer) relayableNFT.deployAddress) (\(RNFT.Transfer ev) -> (guard $ ev.to == nonWeb3Account.address) $> ev.tokenId)
    txh <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress, nonceOf: nonWeb3Account.address, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedMessage { nonce, feeAmount: relayFeeAmount, tokenData: BS.toUTF8 "relay mint (p-e-c sign)!" }
          signedMessage = signRelayedMessage nonWeb3Account.prv msg
      recoveredSigner <- expectRight' =<< recoverRelayedMessageSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` nonWeb3Account.address
      mintRelayed signedMessage txOpts
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded
    tokenId <- joinFiber fEv
    ownerOfToken <- expectRight'' =<< (runWeb3 provider $ RNFT.ownerOf txOpts Latest { tokenId })
    ownerOfToken `shouldEqual` nonWeb3Account.address

  it "can be minted for someone else via mintRelayed, with purescript-eth-core doing the signing for both relayer and relayee" do
    fEv <- forkAff $ awaitEvent provider (eventFilter (Proxy :: Proxy RNFT.Transfer) relayableNFT.deployAddress) (\(RNFT.Transfer ev) -> (guard $ ev.to == nonWeb3Account.address) $> ev.tokenId)
    txh <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress, nonceOf: nonWeb3Account.address, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedMessage { nonce, feeAmount: relayFeeAmount, tokenData: BS.toUTF8 "relay mint (2xp-e-c sign)!" }
          signedMessage = signRelayedMessage nonWeb3Account.prv msg
      recoveredSigner <- expectRight' =<< recoverRelayedMessageSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` nonWeb3Account.address
      rawTx <- mintRelayed' secondNonWeb3Account.prv signedMessage txOpts
      eth_sendRawTransaction rawTx
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded
    tokenId <- joinFiber fEv
    ownerOfToken <- expectRight'' =<< (runWeb3 provider $ RNFT.ownerOf txOpts Latest { tokenId })
    ownerOfToken `shouldEqual` nonWeb3Account.address

  it "can be transferred for someone else via transferRelayed" do
    let secondaryAccount = unsafePartial fromJust $ secondaryAccounts !! 1
        tertiaryAccount = unsafePartial fromJust $ secondaryAccounts !! 2
        password = accountPassword secondaryAccount
    fEv <- forkAff $ awaitEvent provider (eventFilter (Proxy :: Proxy RNFT.Transfer) relayableNFT.deployAddress) (\(RNFT.Transfer ev) -> (guard $ ev.to == secondaryAccount) $> ev.tokenId)
    txh <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress , nonceOf: secondaryAccount, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedMessage { nonce, feeAmount: relayFeeAmount, tokenData: BS.toUTF8 "relay transfer mint!" }
      signedMessage <- signRelayedMessageWeb3 secondaryAccount password msg
      recoveredSigner <- expectRight' =<< recoverRelayedMessageSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` secondaryAccount
      mintRelayed signedMessage txOpts
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded
    tokenID <- resizeUIntN <$> joinFiber fEv
    txh' <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress , nonceOf: secondaryAccount, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedTransfer { nonce, feeAmount: relayFeeAmount, tokenID, destination: tertiaryAccount }
      signedMessage <- signRelayedTransferWeb3 secondaryAccount password msg
      recoveredSigner <- expectRight' =<< recoverRelayedTransferSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` secondaryAccount
      transferRelayed signedMessage txOpts
    TransactionReceipt txr' <- pollTransactionReceipt txh' provider
    txr'.status `shouldEqual` Succeeded
    ownerOfToken <- expectRight'' =<< (runWeb3 provider $ RNFT.ownerOf txOpts Latest { tokenId: resizeUIntN tokenID })
    ownerOfToken `shouldEqual` tertiaryAccount

  it "can be transferred for someone else via transferRelayed, with purescript-eth-core doing the signing" do
    let tertiaryAccount = unsafePartial fromJust $ secondaryAccounts !! 2
    fEv <- forkAff $ awaitEvent provider (eventFilter (Proxy :: Proxy RNFT.Transfer) relayableNFT.deployAddress) (\(RNFT.Transfer ev) -> (guard $ ev.to == nonWeb3Account.address) $> ev.tokenId)
    txh <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress , nonceOf: nonWeb3Account.address, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedMessage { nonce, feeAmount: relayFeeAmount, tokenData: BS.toUTF8 "relay transfer mint!" }
      let signedMessage = signRelayedMessage nonWeb3Account.prv msg
      recoveredSigner <- expectRight' =<< recoverRelayedMessageSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` nonWeb3Account.address
      mintRelayed signedMessage txOpts
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded
    tokenID <- resizeUIntN <$> joinFiber fEv
    txh' <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress , nonceOf: nonWeb3Account.address, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedTransfer { nonce, feeAmount: relayFeeAmount, tokenID, destination: tertiaryAccount }
      let signedMessage = signRelayedTransfer nonWeb3Account.prv msg
      recoveredSigner <- expectRight' =<< recoverRelayedTransferSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` nonWeb3Account.address
      transferRelayed signedMessage txOpts
    TransactionReceipt txr' <- pollTransactionReceipt txh' provider
    txr'.status `shouldEqual` Succeeded
    ownerOfToken <- expectRight'' =<< (runWeb3 provider $ RNFT.ownerOf txOpts Latest { tokenId: resizeUIntN tokenID })
    ownerOfToken `shouldEqual` tertiaryAccount

  it "can be transferred for someone else via transferRelayed, with purescript-eth-core doing the signing for both relayer and relayee" do
    let fourthAccount = unsafePartial fromJust $ secondaryAccounts !! 3
    fEv <- forkAff $ awaitEvent provider (eventFilter (Proxy :: Proxy RNFT.Transfer) relayableNFT.deployAddress) (\(RNFT.Transfer ev) -> (guard $ ev.to == nonWeb3Account.address) $> ev.tokenId)
    txh <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress , nonceOf: nonWeb3Account.address, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedMessage { nonce, feeAmount: relayFeeAmount, tokenData: BS.toUTF8 "relay transfer mint (2xp-e-c)!" }
      let signedMessage = signRelayedMessage nonWeb3Account.prv msg
      recoveredSigner <- expectRight' =<< recoverRelayedMessageSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` nonWeb3Account.address
      mintRelayed signedMessage txOpts
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded
    tokenID <- resizeUIntN <$> joinFiber fEv
    txh' <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress , nonceOf: nonWeb3Account.address, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedTransfer { nonce, feeAmount: relayFeeAmount, tokenID, destination: fourthAccount }
      let signedMessage = signRelayedTransfer nonWeb3Account.prv msg
      recoveredSigner <- expectRight' =<< recoverRelayedTransferSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` nonWeb3Account.address
      rawTx <- transferRelayed' secondNonWeb3Account.prv signedMessage txOpts
      eth_sendRawTransaction rawTx
    TransactionReceipt txr' <- pollTransactionReceipt txh' provider
    txr'.status `shouldEqual` Succeeded
    ownerOfToken <- expectRight'' =<< (runWeb3 provider $ RNFT.ownerOf txOpts Latest { tokenId: resizeUIntN tokenID })
    ownerOfToken `shouldEqual` fourthAccount

  it "can be burned for someone else via transferRelayed" do
    let secondaryAccount = unsafePartial fromJust $ secondaryAccounts !! 1
        password = accountPassword secondaryAccount
    fEv <- forkAff $ awaitEvent provider (eventFilter (Proxy :: Proxy RNFT.Transfer) relayableNFT.deployAddress) (\(RNFT.Transfer ev) -> (guard $ ev.to == secondaryAccount) $> ev.tokenId)
    txh <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress , nonceOf: secondaryAccount, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedMessage { nonce, feeAmount: relayFeeAmount, tokenData: BS.toUTF8 "relay burn mint!" }
      signedMessage <- signRelayedMessageWeb3 secondaryAccount password msg
      recoveredSigner <- expectRight' =<< recoverRelayedMessageSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` secondaryAccount
      mintRelayed signedMessage txOpts
    TransactionReceipt txr <- pollTransactionReceipt txh provider
    txr.status `shouldEqual` Succeeded
    tokenID <- resizeUIntN <$> joinFiber fEv
    txh' <- expectRight' =<< runWeb3 provider do
      nonce <- expectRight' =<< getRelayNonce { rnftAddress: relayableNFT.deployAddress , nonceOf: secondaryAccount, checkFrom: primaryAccount, checkAt: Latest }
      let msg = UnsignedRelayedTransfer { nonce, feeAmount: relayFeeAmount, tokenID, destination: zeroAddress }
      signedMessage <- signRelayedTransferWeb3 secondaryAccount password msg
      recoveredSigner <- expectRight' =<< recoverRelayedTransferSignerWeb3 signedMessage txOpts Latest
      recoveredSigner `shouldEqual` secondaryAccount
      transferRelayed signedMessage txOpts
    TransactionReceipt txr' <- pollTransactionReceipt txh' provider
    txr'.status `shouldEqual` Succeeded
    eOwnerOf <- (runWeb3 provider $ RNFT.ownerOf txOpts Latest { tokenId: resizeUIntN tokenID })
    -- depending on version of ethereum client, openzeppelin, etc., we might either get a revert with
    -- a helpful message, or the zero address
    case eOwnerOf of
      Left (Rpc (RpcError err)) -> err.message `shouldContain` "nonexistent token"
      other -> do
        ownerOfToken <- expectRight'' other
        ownerOfToken `shouldEqual` zeroAddress
