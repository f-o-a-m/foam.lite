module Main where

import Prelude

import Effect (Effect)
import Chanterelle.Internal.Logging (LogLevel(..), log, setLogLevel)

import Lora.UDP.Pkt as Pkt
import Lora.UDP.Server as Server
import Env (mkEnv)
import Lora.FoamBridge

handler :: Server.PktHandler
handler respond pkt = case pkt of
  Pkt.PUSH_DATA { token, mac, json } -> do
    log Info "PUSH_DATA"
    log Info $ show token
    log Info $ show mac
    log Info $ show json
    env <- mkEnv
    performPUSH_DATAAction env json
    respond (Pkt.PUSH_ACK { token })
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
main = do
  setLogLevel Debug
  log Info "🍝"
  Server.start "0.0.0.0" 7000 handler
  log Info "listening to UDP packets at 7000 port"
