module Lora.UDP.Server where

import Prelude

import Effect (Effect)
import Data.Int(round, toNumber, toStringAs, decimal)
import Node.Buffer as BufferMod
import Node.Buffer (Buffer, toString, readString, writeString, size, fromArray, create)
import Node.Encoding (Encoding(ASCII))
import Node.Buffer.Internal as BufferInternal
import Data.Maybe (Maybe(..))
import Data.String (length)
import Node.Buffer.Types (BufferValueType(UInt8, UInt16LE), Octet)
import Node.Datagram (createSocket, bindSocket, SocketType(UDPv4), Socket, SocketInfo, onMessage, send, close)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Effect.Class (class MonadEffect, liftEffect)

import Lora.UDP.Pkt as Pkt

type StopServer = Unit -> Effect Unit
type Respond = Pkt.LoraUDPPkt -> Aff Unit
type PktHandler = (Respond) -> Pkt.LoraUDPPkt -> Aff Unit

start :: String -> Int -> (PktHandler) -> Effect Unit
start addr port handler = do
  socket <- createSocket UDPv4 (Just true)
  liftEffect $ bindSocket socket (Just port) (Just addr)
  onMessage socket (\buf sockInfo -> launchAff_ $ msgHandler socket handler buf sockInfo)

msgHandler :: forall m. MonadAff m => MonadEffect m => Socket -> (PktHandler) -> Buffer -> SocketInfo -> m Unit
msgHandler socket handler buff socketInfo = do
  log Info ""
  log Info $ "received UDP packet from " <> socketInfo.address <> ":" <> (toStringAs decimal socketInfo.port)
  s <- liftEffect $ (show <$> toString ASCII buff)
  log Info s
  maybePkt <- liftEffect $ Pkt.read buff
  case maybePkt of
    Just pkt -> do
      liftAff $ handler (responder socket socketInfo.address socketInfo.port) pkt
    _ -> do
      log Error "unrecognized packet"
      s <- liftEffect $ (show <$> toString ASCII buff)
      log Error s

responder :: forall m. MonadEffect m => Socket -> String -> Int -> Pkt.LoraUDPPkt -> m Unit
responder socket addr port pkt = liftEffect $ do
  log Info "sending pkt"
  buff <- Pkt.write pkt
  send socket buff Nothing Nothing port addr (Just $ log Info "sent")
