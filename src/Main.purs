module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Control.Monad.RWS (RWSResult(..), runRWS)
import Data.Foldable (fold, for_)
import Data.Newtype (wrap)
import Data.String (split)
import Data.Maybe
import Data.List (intercalate)
import Node.ReadLine as ReadLine
import Options.Applicative ((<**>))
import Options.Applicative as Options
import GameItem
import GameSetup (Coordinates(..), GameEnvironment(..), gameEnvironment)
import GameState (GameState(..), initialGameState)
import Game (game)

runGame :: GameEnvironment -> Effect Unit
runGame gameEnv = do
  gameInterface <- ReadLine.createConsoleInterface ReadLine.noCompletion
  printCommands
  ReadLine.setPrompt "> " gameInterface
  let
    lineHandler :: GameState -> String -> Effect Unit
    lineHandler startState input = do
      case runRWS (game (split (wrap " ") input)) gameEnv startState of
        RWSResult state _ written -> do
          for_ written log
          ReadLine.setLineHandler (lineHandler state) $ gameInterface
      ReadLine.prompt gameInterface
      pure unit
  ReadLine.setLineHandler (lineHandler initialGameState) gameInterface
  ReadLine.prompt gameInterface

printCommands :: Effect Unit
printCommands = do
  let
    commands =
      [ "look: Get the location where you are currently."
      , "north: Explore room to the north."
      , "south: Explore room to the south."
      , "east: Explore room to the east."
      , "west: Explore room to the west."
      , "inventory: View the items in your inventory."
      , "take [item]: Add a specific item to your inventory."
      , "use [item]: Use a specific item from your inventory."
      , "cheat : get all the items in the inventory"
      ]
  log "All the commands are case sensitive, make sure you type in correctly."
  log "Start a game taking a 'look'! to find where are you?"
  for_ commands $ \cmd -> log ("- " <> cmd)

main :: Effect Unit
main = Options.customExecParser preferences argParser >>= runGame
  where
  preferences = Options.prefs Options.showHelpOnEmpty

  argParser :: Options.ParserInfo GameEnvironment
  argParser = Options.info (environment <**> Options.helper) parserOptions

  environment :: Options.Parser GameEnvironment
  environment = gameEnvironment <$> player

  player :: Options.Parser String
  player =
    Options.strOption
      $ fold
          [ Options.long "player"
          , Options.short 'p'
          ]

  parserOptions =
    fold
      [ Options.header "Let's start Escape the Mansion game!"
      ]
