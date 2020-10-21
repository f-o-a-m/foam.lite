module DApp.Message where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Array as Array
import Data.ByteString as BS
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List (toUnfoldable) as List
import Data.Maybe (maybe)
import Data.NonEmpty as NE
import Data.Number as Number
import Data.String.CodeUnits (fromCharArray) as String
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf1, chooseInt)
import Text.Parsing.Parser (ParseError, fail, runParser)
import Text.Parsing.Parser.Combinators (many1Till)
import Text.Parsing.Parser.String (anyChar, char, eof, string)

data DAppMessage =
    Location { lat :: Number, lon :: Number }
  | ArbitraryString String
  | LocationWithArbitrary { lat :: Number, lon :: Number, string :: String }

instance encodeJsonDAppMessage :: EncodeJson DAppMessage where
  encodeJson (Location l) = encodeJson l
  encodeJson (ArbitraryString s) = encodeJson { string: s }
  encodeJson (LocationWithArbitrary la) = encodeJson la

derive instance eqDAppMessage :: Eq DAppMessage

derive instance genericDAppMessage :: Generic DAppMessage _
instance showDAppMessage :: Show DAppMessage where
  show (Location { lat, lon }) = "L:" <> show lat <> "," <> show lon
  show (ArbitraryString s) = "A:" <> s
  show (LocationWithArbitrary { lat, lon, string }) = "M:"  <> show lat <> "," <> show lon <> "," <> string

packDAppMessage :: DAppMessage -> BS.ByteString
packDAppMessage = BS.toUTF8 <<< show

parseDAppMessage :: String -> Either ParseError DAppMessage
parseDAppMessage = flip runParser $ (parseArbitrary <|> parseLocationWithArbitrary <|> parseLocation <|> (fail "Unparsable DAppMessage"))
  where remainingString' = many1Till anyChar eof
        remainingString = (String.fromCharArray <<< List.toUnfoldable) <$> remainingString'
        parseNumber name s = maybe (fail $ "Could not parse " <> name) pure (Number.fromString <<< String.fromCharArray $ List.toUnfoldable s)
        parseLocation = do
          void $ string "L:"
          lat <- (parseNumber "latitude") =<< many1Till anyChar (char ',')
          lon <- (parseNumber "longitude") =<< remainingString'
          pure $ Location { lat, lon }
        parseArbitrary = do
          void $ string "A:"
          s <- remainingString
          pure $ ArbitraryString s
        parseLocationWithArbitrary = do
          void $ string "M:"
          lat <- (parseNumber "latitude") =<< many1Till anyChar (char ',')
          lon <- (parseNumber "longitude") =<< many1Till anyChar (char ',')
          string <- remainingString
          pure $ LocationWithArbitrary { lat, lon, string }

instance arbitraryDAppMessage :: Arbitrary DAppMessage where
  arbitrary = do
    which <- chooseInt 1 3
    case which of
      1 -> genLocation
      2 -> genArbitrary
      3 -> genLocationWithArb
      _ -> genArbitrary -- impossible as chooseInt is within the int domain of [1, 3], but whatever
    where
      genArbStr = do
        nea <- arrayOf1 arbitrary
        -- limit our arb string length
        let arr = Array.take 40 $ Array.cons (NE.head nea) (NE.tail nea)
        pure $ String.fromCharArray arr
      genArbitrary = ArbitraryString <$> genArbStr
      genLocation  = do
        lat <- arbitrary
        lon <- arbitrary
        pure $ Location { lat, lon }
      genLocationWithArb = do
        lat <- arbitrary
        lon <- arbitrary
        string <- genArbStr
        pure $ LocationWithArbitrary { lat, lon, string }

    