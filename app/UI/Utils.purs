module UI.Utils where

import Prelude

import Network.Ethereum.Core.HexString (HexString, takeHex)
import Network.Ethereum.Core.Signatures (Address, unAddress)

formatHexString :: HexString -> String
formatHexString a = show (takeHex 4 a) <> "..."

formatAddress :: Address -> String
formatAddress = formatHexString <<< unAddress
