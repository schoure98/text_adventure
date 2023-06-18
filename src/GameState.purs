module GameState where

import Prelude
import GameItem
import GameSetup (Coordinates(..), RoomName, getRoomName)
import Data.Map as M
import Data.Set as S
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

data GameState
  = GameState
    { items :: M.Map Coordinates (S.Set GameItem)
    , playerPosition :: Coordinates
    , inventory :: S.Set GameItem
    , currentRoom :: String
    }

instance showGameState :: Show GameState where
  show (GameState gameState) =
    "Current Game State "
      <> "You are in the "
      <> show gameState.currentRoom
      <> ", You have these items: "
      <> show gameState.items
      <> ", and the inventory includes: "
      <> show gameState.inventory
      <> " }"

initialGameState :: GameState
initialGameState =
  GameState
    { items:
        M.fromFoldable
          [ Tuple (Coordinates { x: 0, y: 1 }) (S.singleton Keycard)
          , Tuple (Coordinates { x: 0, y: 2 }) (S.singleton Flashlight)
          , Tuple (Coordinates { x: 1, y: 0 }) (S.singleton Screwdriver)
          , Tuple (Coordinates { x: 1, y: 1 }) (S.empty)
          , Tuple (Coordinates { x: 1, y: 2 }) (S.empty)
          , Tuple (Coordinates { x: 2, y: 0 }) (S.empty)
          , Tuple (Coordinates { x: 2, y: 1 }) (S.empty)
          , Tuple (Coordinates { x: -1, y: 1 }) (S.singleton Diary)
          , Tuple (Coordinates { x: 2, y: 1 }) (S.singleton GlassBottle)
          ]
    , playerPosition: Coordinates { x: 0, y: 0 }
    , inventory: S.empty
    , currentRoom: getRoomName (Coordinates { x: 0, y: 0 })
    }
