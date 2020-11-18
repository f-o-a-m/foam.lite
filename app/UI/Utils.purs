module UI.Utils where

import Prelude

import Data.Array (elem, foldl, nubByEq)
import Data.Bifunctor (lmap, rmap)
import Data.String (Pattern(..), null, split)
import Data.String.CodeUnits (length, take, drop)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Properties as HP
import Network.Ethereum.Core.HexString (HexString, takeHex)
import Network.Ethereum.Core.Signatures (Address, unAddress)
import Unsafe.Coerce (unsafeCoerce)

formatHexString :: HexString -> String
formatHexString a = show (takeHex 4 a) <> "..."

formatAddress :: Address -> String
formatAddress = formatHexString <<< unAddress

-- Everything below is vendored from Ocelot.HTML.Properties,
-- as we ended up using it a lot before getting rid of the dependency on it

css
  :: ∀ r i
   . String
  -> HH.IProp ( class :: String | r) i
css = HP.class_ <<< HH.ClassName

appendIProps
  :: ∀ r i
   . Array (HH.IProp (class :: String | r) i)
  -> Array (HH.IProp (class :: String | r) i)
  -> Array (HH.IProp (class :: String | r) i)
appendIProps ip ip' =
  iprops <> iprops' <> classNames
  where
    (Tuple classes iprops) = extract ip
    (Tuple classes' iprops') = extract ip'
    classNames =
      pure
      <<< HP.classes
        $ HH.ClassName
      <$> nubByEq
          (\c c' -> classify c == classify c')
          (classes' <> classes)

infixr 5 appendIProps as <&>

extract
  :: ∀ r i
   . Array (HH.IProp r i)
  -> Tuple (Array String) (Array (HH.IProp r i))
extract =
  foldl f (Tuple [] [])
  where
    f acc (HP.IProp (HHC.Property "className" className)) =
      lmap (_ <> (split (Pattern " ") $ coerceClassName className)) acc
    f acc iprop = rmap (_ <> [iprop]) acc

    coerceClassName :: HHC.PropValue -> String
    coerceClassName = unsafeCoerce
-- ditto
classify
  :: String
  -> String
classify str
  | startsWith "p" str && not null (classifySide $ drop 1 str)
    = "padding" <-> classifySide (drop 1 str)
  | startsWith "m" str && not null (classifySide $ drop 1 str)
    = "margin" <-> classifySide (drop 1 str)
  | startsWith "-m" str && not null (classifySide $ drop 2 str)
    = "margin" <-> classifySide (drop 2 str)
  | startsWith "min-" str = "min" <-> classify (drop 4 str)
  | startsWith "max-" str = "max" <-> classify (drop 4 str)
  | startsWith "w-" str = "width"
  | startsWith "h-" str = "height"
  | startsWith "overflow-" str && (classifyOverflow $ drop 9 str) /= drop 9 str
    = "overflow" <-> (classifyOverflow $ drop 9 str)
  | otherwise = str

classifyOverflow
  :: String
  -> String
classifyOverflow str
  | startsWith "x-" str = "horizontal" <-> (classifyOverflow $ drop 2 str)
  | startsWith "y-" str = "vertical" <-> (classifyOverflow $ drop 2 str)
  | elem str ["auto", "hidden", "visible", "scroll"] = ""
  | otherwise = str


classifySide
  :: String
  -> String
classifySide str
  | startsWith "t-" str = "top"
  | startsWith "r-" str = "right"
  | startsWith "b-" str = "bottom"
  | startsWith "l-" str = "left"
  | startsWith "x-" str = "horizontal"
  | startsWith "y-" str = "vertical"
  | startsWith "-" str = "all"
  | otherwise = ""

append'
  :: String
  -> String
  -> String
append' x "" = x
append' x y  = x <> "-" <> y

infixr 5 append' as <->

startsWith
  :: String
  -> String
  -> Boolean
startsWith str0 str1 = str0 == (take (length str0) str1)