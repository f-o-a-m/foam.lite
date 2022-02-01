module Main where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log, setLogLevel)
import DApp.Support (mkEnv, AppEnv)
import Effect (Effect)
import Data.Maybe (fromMaybe)
import Node.Process (lookupEnv)
import Data.Int (fromString)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)

import Lora.FoamBridge (performPUSH_DATAAction)
import Lora.UDP.Pkt as Pkt
import Lora.UDP.Server as Server

handler :: AppEnv -> Server.PktHandler
handler env respond pkt = case pkt of
  Pkt.PUSH_DATA { token, mac, json } -> do
    log Info "PUSH_DATA"
    log Info $ show token
    log Info $ show mac
    log Info $ show json
    respond (Pkt.PUSH_ACK { token })
    performPUSH_DATAAction env json
  Pkt.PUSH_ACK { token } -> do
    log Info "PUSH_ACK"
    log Info $ show token
  Pkt.PULL_DATA { token, mac } -> do
    log Info "PULL_DATA"
    log Info $ show token
    log Info $ show mac
    respond (Pkt.PULL_ACK { token })
  _ -> do
    log Error "unimplemented packet"

main :: Effect Unit
main = launchAff_ do
  env <- mkEnv
  liftEffect $ setLogLevel Debug

  addr <- fromMaybe "0.0.0.0" <$> (liftEffect $ lookupEnv "PACKET_RECEIVER_ADDRESS")
  port <- fromMaybe 7000 <$> do
    maybePort <- liftEffect $ lookupEnv "PACKET_RECEIVER_PORT"
    pure $ fromString =<< maybePort

  liftEffect $ Server.start addr port (handler env)
  log Info $ "listening to UDP packets at " <> addr <> ":" <> (show port)
