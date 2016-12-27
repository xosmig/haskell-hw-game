
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Lib

import UI.NCurses
import Data.Maybe
import Control.Monad

newtype ConsoleUI a = ConsoleUI  { runConsoleUI :: Curses a }
  deriving (Functor, Applicative, Monad)

moveCursorTo w (x, y) = updateWindow w (moveCursor (toInteger x) (toInteger y))

drawHero w pos = do
  moveCursorTo w pos
  updateWindow w (drawString "@")
  moveCursorTo w pos

instance GameUI ConsoleUI where
  -- nextStep :: ui Direction
  nextStep = do
    mbEvent <- ConsoleUI $ do
      w <- defaultWindow
      getEvent w Nothing
    case mbEvent of
      Just event -> case event of
        EventSpecialKey KeyUpArrow    -> return $ Just North
        EventSpecialKey KeyDownArrow  -> return $ Just South
        EventSpecialKey KeyLeftArrow  -> return $ Just West
        EventSpecialKey KeyRightArrow -> return $ Just East
        EventCharacter 'q' -> return Nothing
        _ -> nextStep
      Nothing -> nextStep

  -- movePlayer :: Position -> ui ()
  movePlayer pos = ConsoleUI $ do
    w <- defaultWindow
    updateWindow w (drawString " ")
    drawHero w pos
    render

field =
  " #x     \n\
  \   ###  \n\
  \   #    \n\
  \  ###   \n\
  \        "

startPos = (0, 0)

stateExample :: GameState
stateExample = fromJust $ gameState field startPos

showMessage w message = do
  updateWindow w clear
  moveCursorTo w (0, 0)
  updateWindow w (drawString message)

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w (drawString field)
    drawHero w startPos
    render
    state <- runConsoleUI $ execGameT playGame stateExample
    showMessage w $ show $ gsStatus state
    render
    wait
  where
    wait = do
      w <- defaultWindow
      mbEvent <- getEvent w Nothing
      case mbEvent of
        Just event -> case event of
          EventSpecialKey _ -> return ()
          EventCharacter  _ -> return ()
          _ -> wait
        Nothing -> wait
