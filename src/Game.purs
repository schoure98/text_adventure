module Game where

import Prelude
import Control.Monad.RWS (RWS)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify_, put)
import Control.Monad.Writer (tell)
import Data.Foldable (for_, foldr, traverse_)
import Data.Traversable (traverse_)
import Data.Maybe (fromMaybe)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import GameItem
import GameSetup (Coordinates(..), printCoordinates, getRoomName, GameEnvironment(..))
import GameState

type GameLog
  = L.List String

type Game
  = RWS GameEnvironment GameLog GameState

describeRoom :: Game Unit
describeRoom = do
  GameState state <- get
  case state.playerPosition of
    Coordinates { x: 0, y: 0 } -> do
      tell (L.singleton "You are in a dimly lit room. Search for clues and items.")
      tell (L.singleton "You find yourself trapped in an abandoned mansion.")
    Coordinates { x: 0, y: 1 } -> do
      tell (L.singleton "You are in a small hallway. There is a Keycard on the floor.")
      tell (L.singleton "Keycard might help to unlock the door and escape.")
    Coordinates { x: 0, y: 2 } -> do
      tell (L.singleton "You have entered a storage room. You spot a Flashlight on the shelves.")
      tell (L.singleton "Take the Flashlight to illuminate the darkness and navigate the mansion.")
    Coordinates { x: 1, y: 0 } -> do
      tell (L.singleton "You are in a workshop. There is a sturdy Screwdriver on a workbench.")
      tell (L.singleton "Grab the Screwdriver as it might come in handy for unexpected situations.")
    Coordinates { x: -1, y: 0 } -> do
      tell (L.singleton "You find yourself in a living room. Search for hidden compartments or passages.")
      tell (L.singleton "Although the cozy sofa tempts you, remember that your escape mission comes first.")
    Coordinates { x: 1, y: 1 } -> do
      tell (L.singleton "You step into a bedroom. Inspect meticulously for hidden objects or clues.")
      tell (L.singleton "A neatly made bed stands against the wall, untouched for ages.")
      tell (L.singleton "If you fail to find the clue, don't forget to change the way.")
    Coordinates { x: 2, y: 0 } -> do
      tell (L.singleton "You enter a kitchen. Search thoroughly for vital ingredients or tools.")
      tell (L.singleton "Pots and pans lie scattered in a messy way, indicating a chaotic meal preparation.")
      tell (L.singleton "There seems like no food left at all! When you escape, grab something from outside instead?")
    Coordinates { x: 2, y: 1 } -> do
      tell (L.singleton "You find yourself in a dining area and there's a GlassBottle in front of you.")
      tell (L.singleton "Grab some water and get ready to escape the mansion faster!")
    Coordinates { x: -1, y: 1 } -> do
      tell (L.singleton "You are in a study room. Read the Diary on the desk to discover the mansion's history.")
      tell (L.singleton "The Diary might also provide hints for your escape. Pay attention to its contents.")
    Coordinates { x: -1, y: 2 } -> do
      tell (L.singleton "You have reached the Escape Door. Check your bag for the required items.")
    _ -> tell (L.singleton "You are in an unknown room.")

itemPickUp :: GameItem -> Game Unit
itemPickUp item = do
  GameState state <- get
  if item `S.member` state.inventory then
    tell (L.singleton "You already have this item.")
  else case state.playerPosition `M.lookup` state.items of
    Just items
      | item `S.member` items -> do
        let
          newItems = M.update (Just <<< S.delete item) state.playerPosition state.items

          newInventory = S.insert item state.inventory
        put
          $ GameState
              state
                { items = newItems
                , inventory = newInventory
                }
        tell (L.singleton ("Now you have the " <> show item))
    _ -> tell (L.singleton "I don't see that item here.")

changeRoom :: Int -> Int -> Game Unit
changeRoom dx dy = do
  modify_ (\(GameState state) -> GameState (state { playerPosition = updateCoords state.playerPosition }))
  describeRoom
  where
  updateCoords :: Coordinates -> Coordinates
  updateCoords (Coordinates { x, y }) = Coordinates { x: x + dx, y: y + dy }

has :: GameItem -> Game Boolean
has item = do
  GameState state <- get
  pure $ item `S.member` state.inventory

use :: GameItem -> Game Unit
use Keycard = do
  GameState state <- get
  if S.size (state.inventory) >= 5 then
    tell (L.singleton "You insert the keycard into the slot. The door opens.")
  else
    tell (L.singleton "The escape is not so easy, there's more adventure for you in here!")

use Flashlight = tell (L.singleton "You turn on the flashlight. The room is illuminated.")

use Screwdriver = tell (L.singleton "You use the screwdriver to tighten the loose screws.")

use Diary = tell (L.singleton "You open the diary and read its contents.")

use GlassBottle = tell (L.singleton "You examine the GlassBottle. It's empty.")

use item = tell (L.singleton $ "You cannot use the " <> show item <> " right now.")

cheat :: Game Unit
cheat = do
  GameState state <- get
  let
    allItems = foldr S.union S.empty state.items

    newInventory = S.union allItems state.inventory
  put $ GameState (state { items = M.empty, inventory = newInventory })
  tell (L.singleton $ "Cheat Activated: find everthing in inventory!")

game :: Array String -> Game Unit
game [ "look" ] = do
  GameState state <- get
  describeRoom
  for_ (M.lookup state.playerPosition state.items)
    $ \items ->
        tell (map (\item -> "You can see the " <> show item <> ".") (S.toUnfoldable items :: L.List GameItem))

game [ "north" ] = changeRoom 0 1

game [ "south" ] = changeRoom 0 (-1)

game [ "west" ] = changeRoom (-1) 0

game [ "east" ] = changeRoom 1 0

game [ "inventory" ] = do
  GameState state <- get
  tell (map (\item -> "You have the " <> show item <> ".") (S.toUnfoldable state.inventory :: L.List GameItem))

game [ "take", item ] = case readItem item of
  Nothing -> tell (L.singleton "I don't understand what you are saying.")
  Just gameItem -> itemPickUp gameItem

game [ "use", item ] = case readItem item of
  Nothing -> tell (L.singleton "I don' think that's in here.")
  Just gameItem -> do
    hasItem <- has gameItem
    if hasItem then
      use gameItem
    else
      tell (L.singleton "You don't have that item.")

game [ "cheat" ] = cheat

game [] = pure unit

game _ = tell (L.singleton "I don't understand.")
