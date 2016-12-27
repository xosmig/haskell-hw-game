
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Lib

import UI.NCurses
import Data.Maybe

newtype ConsoleUI a = ConsoleUI  { runConsoleUI :: Curses a }
  deriving (Functor, Applicative, Monad)

instance GameUI ConsoleUI where
  -- nextStep :: ui Direction
  nextStep = do
      mbEvent <- ConsoleUI $ defaultWindow >>= \w -> getEvent w Nothing
      case mbEvent of
        Just event -> case event of
          EventSpecialKey KeyUpArrow    -> return $ Just North
          EventSpecialKey KeyDownArrow  -> return $ Just South
          EventSpecialKey KeyLeftArrow  -> return $ Just West
          EventSpecialKey KeyRightArrow -> return $ Just East
          EventCharacter 'q' -> return Nothing
          _ -> nextStep
        Nothing -> nextStep

  -- movePlayer :: Position -> Position -> ui ()
  movePlayer oldPos newPos = pure ()


fieldExample =
  " #x     \n\
  \   ###  \n\
  \   #    \n\
  \  ###   \n\
  \        "

stateExample :: GameState
stateExample = fromJust $ gameState fieldExample (0, 0)

-- run ds = evalFakeUI (execGameT playGame stateExample) ds

main :: IO ()
main = runCurses $ do
  setEcho False
  runConsoleUI $ evalGameT playGame stateExample
