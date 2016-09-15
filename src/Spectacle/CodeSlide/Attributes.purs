module Spectacle.CodeSlide.Attributes where

import Data.Foreign (Foreign, toForeign)
import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude (($), (<>), (<$>), (-))
import Pux.Html (Attribute)
import Pux.Html.Attributes (attr)

code :: forall a. String -> Attribute a
code = attr "code"

data Range = Range Loc (Array RangeOption)
data Loc = Loc Int Int
data RangeOption
  = Title String
  | Note String
  | Image String

ranges :: forall a. Array Range -> Attribute a
ranges rs = attr "ranges" (rangeToJS <$> rs)
  where
    rangeToJS :: Range -> StrMap Foreign
    rangeToJS (Range (Loc x y) options) =
      fromFoldable $
        [ "loc" /\ toForeign [x - 1, y] ] <> (rangeOptionToTuple <$> options)

    rangeOptionToTuple :: RangeOption -> Tuple String Foreign
    rangeOptionToTuple (Title title) = "title" /\ toForeign title
    rangeOptionToTuple (Note note) = "note" /\ toForeign note
    rangeOptionToTuple (Image image) = "image" /\ toForeign image
