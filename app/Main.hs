
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Lib

import UI.NCurses
import Data.Maybe
import Control.Monad
import qualified Data.Vector as V
import Data.Vector((!))

newtype ConsoleUI a = ConsoleUI  { runConsoleUI :: Curses a }
  deriving (Functor, Applicative, Monad)

moveCursorTo w (x, y) = updateWindow w (moveCursor (toInteger x) (toInteger y))
drawStringHere w s = updateWindow w (drawString s)

drawHero w (x, y) = do
  let pos = (x + 1, y + 1)
  moveCursorTo w pos
  drawStringHere w "@"
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
    drawStringHere w " "
    drawHero w pos
    render

field = fromJust $ toField
  " #x     \n\
  \   ###  \n\
  \   #    \n\
  \  ###   \n\
  \        "

drawField :: Window -> Field -> Curses ()
drawField w field = unless (V.length field == 0) $ do
    moveCursorTo w (0, 0)
    drawStringHere w borderLine
    forM_ field $ \row -> do
      drawStringHere w "^"
      forM_ row $ \cell ->
        drawStringHere w [cellToChar cell]
      drawStringHere w "^\n"
    drawStringHere w borderLine
    drawStringHere w rules
  where
    width = V.length $ field ! 0
    borderLine = replicate (width + 2) '^' ++ "\n"
    enumerate v = zip [0..] (V.toList v)
    rules = " q - exit\n\
            \ arrows - move"

startPos = (0, 0)

showMessage w message = do
  updateWindow w clear
  moveCursorTo w (0, 0)
  updateWindow w (drawString message)

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    drawField w field
    drawHero w startPos
    render
    state <- runConsoleUI $ execGameT playGame field startPos
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
