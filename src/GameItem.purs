module GameItem where

import Prelude
import Data.Maybe (Maybe(..))

-- Game items
data GameItem
  = Keycard
  | Flashlight
  | Screwdriver
  | Diary
  | GlassBottle

instance showGameItem :: Show GameItem where
  show Keycard = "Keycard"
  show Flashlight = "Flashlight"
  show Screwdriver = "Screwdriver"
  show Diary = "Diary"
  show GlassBottle = "Glass Bottle"

derive instance eqGameItem :: Eq GameItem

derive instance ordGameItem :: Ord GameItem

readItem :: String -> Maybe GameItem
readItem "Keycard" = Just Keycard

readItem "Flashlight" = Just Flashlight

readItem "Screwdriver" = Just Screwdriver

readItem "Diary" = Just Diary

readItem "Glass Bottle" = Just GlassBottle

readItem _ = Nothing
