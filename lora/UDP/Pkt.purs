module Lora.UDP.Pkt
  (
  LoraUDPPkt(..),
  GatewayMac(..),
  ProtocolToken(..),
  read,
  write,
  parseLoraUDPPkt
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Trans.Class (lift)
import Data.Argonaut as A
import Data.ArrayBuffer.DataView (buffer, whole)
import Data.ByteString as BS
import Data.Either (Either(..), either, hush)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect (Effect)
import Node.Buffer (class MutableBuffer, Buffer, create, fromArray, readString, size, toArrayBuffer, writeString)
import Node.Buffer as BufferMod
import Node.Buffer.Internal as BufferInternal
import Node.Buffer.Types (BufferValueType(UInt8, Int16BE), Octet)
import Node.Encoding (Encoding(ASCII))
import Text.Parsing.Parser (fail, runParserT)
import Text.Parsing.Parser.DataView (anyInt16be, satisfyInt8, takeN, takeRest)
import Effect.Console (log)

import Lora.UDP.PushDataJSON (PushDataJSON(..))

newtype ProtocolToken = ProtocolToken Int

derive instance genericProtocolToken :: Generic ProtocolToken _
instance showProtocolToken :: Show ProtocolToken where
  show = genericShow

readProtocolToken :: forall m buf. MutableBuffer buf m => Int -> buf -> m ProtocolToken
readProtocolToken ofs buff = ProtocolToken <<< round <$> (BufferInternal.read Int16BE ofs buff)

writeProtocolToken :: forall m buf. MutableBuffer buf m => ProtocolToken -> Int -> buf -> m Unit
writeProtocolToken (ProtocolToken token) ofs = BufferMod.write Int16BE (toNumber token) ofs

-- ------------------------------------------------------

newtype GatewayMac = GatewayMac Buffer
instance showGatewayMac :: Show GatewayMac where
  show (GatewayMac mac) = "GatewayMac " <> BS.toString (BS.unsafeFreeze mac) BS.Hex

readGatewayMac :: Int -> Int -> Buffer -> Effect GatewayMac
readGatewayMac from to buff = do
  mac <- BufferMod.create (to - from)
  _ <- BufferInternal.copy from to buff 0 mac
  pure $ GatewayMac mac

writeGatewayMac :: GatewayMac -> Int -> Buffer -> Effect Int
writeGatewayMac (GatewayMac mac) ofs = BufferInternal.copy 0 8 mac ofs

data LoraUDPPkt
    = PUSH_DATA
      { token :: ProtocolToken
      , mac   :: GatewayMac
      , json  :: PushDataJSON
      }
    | PULL_DATA
      { token :: ProtocolToken
      , mac   :: GatewayMac
      }
    | PUSH_ACK { token :: ProtocolToken }
    | PULL_ACK { token :: ProtocolToken }

derive instance genericLoraUDPPkt :: Generic LoraUDPPkt _
instance showLoraUDPPkt :: Show LoraUDPPkt where
  show = genericShow

read :: Buffer -> Effect (Maybe LoraUDPPkt)
read b = do
  loraType <- decodeLoraPktType b
  case loraType of
    Just 0 -> readPUSH_DATA b
    Just 1 -> readPUSH_ACK b
    Just 2 -> readPULL_DATA b
    _ -> pure Nothing

parseLoraUDPPkt :: Buffer -> Effect (Maybe LoraUDPPkt)
parseLoraUDPPkt buf = do
  arrayBuf <- toArrayBuffer buf
  let dataview = whole arrayBuf
  parseRes <- runParserT dataview parseUDPPacket
  pure $ hush parseRes

  where
    parseUDPPacket = do
      _ <- parseVersion
      token <- parseToken
      parsePacketBody token

    matchByte n     = void $ satisfyInt8 (_ == n)
    parseVersion    = matchByte 2
    parseToken      = ProtocolToken <$> anyInt16be
    parseGatewayMac = GatewayMac <$> (dataViewToBuffer =<< takeN 8)

    dataViewToBuffer = lift <<< BufferMod.fromArrayBuffer <<< buffer
    bufferToString   = lift <<< BufferMod.toString ASCII

    parsePacketBody token =  parsePUSH_DATA token
                         <|> parsePUSH_ACK token
                         <|> parsePULL_DATA token
                         <|> parsePULL_RESP token
                         <|> parsePULL_ACK token
                         <|> fail "No valid body parsed!"

    parseJSONString str =
      either (fail <<< show) pure $ A.decodeJson =<< A.parseJson str

    parsePUSH_DATA token = do
      matchByte 0x00
      mac <- parseGatewayMac
      jsonString <- bufferToString =<< dataViewToBuffer =<< takeRest
      json <- parseJSONString jsonString
      pure $ PUSH_DATA { token, mac, json }

    parsePUSH_ACK token = do
      matchByte 0x01
      pure $ PUSH_ACK { token }

    parsePULL_DATA token = do
      matchByte 0x02
      mac <- parseGatewayMac
      pure $ PULL_DATA { token, mac }

    parsePULL_RESP _token = do
      matchByte 0x03
      fail "PULL_RESP packets are currently unsupported!"

    parsePULL_ACK token = do
      matchByte 0x04
      pure $ PUSH_ACK { token }

readPUSH_DATA :: Buffer -> Effect (Maybe LoraUDPPkt)
readPUSH_DATA buff = do
  len <- size buff
  if len < 14 then
    pure Nothing
  else do
    token <- readProtocolToken 1 buff
    mac <- readGatewayMac 4 11 buff
    jsonString <- (readString ASCII 12 len buff)
    let ePushDataJson = do
          json <- A.parseJson jsonString >>= A.decodeJson
          pure $ PUSH_DATA { token, mac, json }
    case ePushDataJson of
      Left err -> log $ show err
      _ -> log "ok"
    pure $ hush ePushDataJson

readPUSH_ACK :: Buffer -> Effect (Maybe LoraUDPPkt)
readPUSH_ACK buff = do
  len <- size buff
  if len < 4 then
    pure Nothing
  else do
    token <- readProtocolToken 1 buff
    pure $ Just $ PUSH_ACK { token }

readPULL_DATA :: Buffer -> Effect (Maybe LoraUDPPkt)
readPULL_DATA buff = do
  len <- size buff
  if len < 12 then
    pure Nothing
  else do
    token <- readProtocolToken 1 buff
    mac <- readGatewayMac 4 11 buff
    pure $ Just $ PULL_DATA { token, mac }

write :: LoraUDPPkt -> Effect Buffer
write (PUSH_DATA { token, mac, json: PushDataJSON jsonRecord }) = do
  let jsonString = A.stringify $ A.encodeJson jsonRecord
      jsonLen = length jsonString
      buffLen = jsonLen + 11
  buff <- create buffLen
  BufferMod.write UInt8 2.0 0 buff
  writeProtocolToken token 1 buff
  BufferMod.write UInt8 0.0 3 buff
  _ <- writeGatewayMac mac 4 buff -- TODO check result
  _ <- writeString ASCII 12 jsonLen jsonString buff -- TODO check result
  pure buff

write (PUSH_ACK { token }) = do
  buff <- (fromArray :: Array Octet -> Effect Buffer) [2, 0, 0, 1]
  writeProtocolToken token 1 buff
  pure buff

write (PULL_DATA _) = create 0 -- TODO

write (PULL_ACK { token }) = do
   buff <- (fromArray :: Array Octet -> Effect Buffer) [2, 0, 0, 4]
   writeProtocolToken token 1 buff
   pure buff

bufLoraVersion :: Buffer -> Effect Int
bufLoraVersion b = round <$> (BufferInternal.read UInt8 0 b)

bufLoraTypeIdentifier :: Buffer -> Effect Int
bufLoraTypeIdentifier b = round <$> (BufferInternal.read UInt8 3 b)

decodeLoraPktType :: Buffer -> Effect (Maybe Int)
decodeLoraPktType buff = do
  len <- size buff
  if len < 4 then
    pure Nothing
  else do
    version <- bufLoraVersion buff
    case version of
      2 -> Just <$> bufLoraTypeIdentifier buff
      _ -> pure Nothing
