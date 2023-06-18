module GameSetup where

import Prelude
import Data.Maybe

-- Coordinates to find player's current position
data Coordinates
  = Coordinates
    { x :: Int
    , y :: Int
    }

instance showCoordinates :: Show Coordinates where
  show (Coordinates cords) = "<" <> show cords.x <> ", " <> show cords.y <> ">"

derive instance equalCoordinates :: Eq Coordinates

derive instance ordCoordinates :: Ord Coordinates

left :: Coordinates -> Coordinates
left (Coordinates { x, y }) = Coordinates { x: x - 1, y }

right :: Coordinates -> Coordinates
right (Coordinates { x, y }) = Coordinates { x: x + 1, y }

up :: Coordinates -> Coordinates
up (Coordinates { x, y }) = Coordinates { x, y: y - 1 }

down :: Coordinates -> Coordinates
down (Coordinates { x, y }) = Coordinates { x, y: y + 1 }

printCoordinates :: Coordinates -> String
printCoordinates (Coordinates coordinates) = "<" <> show coordinates.x <> ", " <> show coordinates.y <> ">"

-- mapping of coordinates with room name
data RoomName
  = BigOldMansion
  | Hallway
  | StorageRoom
  | Workshop
  | LivingRoom
  | Bedroom
  | Kitchen
  | DiningArea
  | StudyRoom

instance showRoomName :: Show RoomName where
  show BigOldMansion = "Big Old Mansion"
  show Hallway = "Hallway"
  show StorageRoom = "Storage Room"
  show Workshop = "Workshop"
  show LivingRoom = "Living Room"
  show Bedroom = "Bedroom"
  show Kitchen = "Kitchen"
  show DiningArea = "Dining Area"
  show StudyRoom = "Study Room"

derive instance eqRoom :: Eq RoomName

derive instance ordRoom :: Ord RoomName

getRoomName :: Coordinates -> String
getRoomName (Coordinates { x: 0, y: 0 }) = "Big Old Mansion"

getRoomName (Coordinates { x: 0, y: 1 }) = "Hallway"

getRoomName (Coordinates { x: 0, y: 2 }) = "Storage Room"

getRoomName (Coordinates { x: 1, y: 0 }) = "Workshop"

getRoomName (Coordinates { x: -1, y: 0 }) = "Living Room"

getRoomName (Coordinates { x: 1, y: 1 }) = "Bedroom"

getRoomName (Coordinates { x: 2, y: 0 }) = "Kitchen"

getRoomName (Coordinates { x: 2, y: 1 }) = "Dining Area"

getRoomName (Coordinates { x: -1, y: 1 }) = "Study Room"

getRoomName (Coordinates { x: -1, y: 2 }) = "Escape Door"

getRoomName _ = "Unknown"

-- Getting player name and boolean debug option
type PlayerName
  = String

newtype GameEnvironment
  = GameEnvironment
  { playerName :: PlayerName
  }

gameEnvironment :: PlayerName -> GameEnvironment
gameEnvironment playerName =
  GameEnvironment
    { playerName: playerName
    }
