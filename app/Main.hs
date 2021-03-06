
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Lib

import UI.NCurses
import Data.Maybe
import Control.Monad
import qualified Data.Vector as V
import Data.Vector((!))
import Control.Monad.State
import System.IO


newtype TextUI a = TextUI { runTextUI :: IO a }
  deriving (Functor, Applicative, Monad)

instance GameUI TextUI where
  -- initialize :: ui ()
  initialize = TextUI $ do
    putStrLn " use wasd to move, q to exit"
    putStrLn "field : "
    putStrLn fieldString
    putStrLn $ "You are at the position " ++ show startPos

  -- nextStep :: ui Direction
  nextStep = do
    ch <- TextUI getChar
    case ch of
      'w' -> return $ Just North
      'a' -> return $ Just West
      's' -> return $ Just South
      'd' -> return $ Just East
      'q' -> return Nothing
      _ -> nextStep

  -- movePlayer :: Position -> ui ()
  movePlayer pos = TextUI $ putStrLn $ "You are at the position " ++ show pos


newtype CursesUI a = CursesUI  { runCursesUI :: Curses a }
  deriving (Functor, Applicative, Monad)

moveCursorTo w (x, y) = updateWindow w (moveCursor (toInteger x) (toInteger y))
drawStringHere w s = updateWindow w (drawString s)

drawHero w (x, y) = do
  let pos = (x + 1, y + 1)
  moveCursorTo w pos
  drawStringHere w "@"
  moveCursorTo w pos

instance GameUI CursesUI where
  -- initialize :: ui ()
  initialize = CursesUI $ do
    w <- defaultWindow
    drawField w field
    drawHero w startPos
    render

  -- nextStep :: ui Direction
  nextStep = do
    mbEvent <- CursesUI $ do
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
  movePlayer pos = CursesUI $ do
    w <- defaultWindow
    drawStringHere w " "
    drawHero w pos
    render

fieldString =
  " #x     \n\
  \   ###  \n\
  \   #    \n\
  \  ###   \n\
  \        "

field = fromJust $ toField fieldString

drawField :: Window -> Field -> Curses ()
drawField w field = unless (V.length field == 0) $ do
    moveCursorTo w (0, 0)
    drawStringHere w borderLine
    forM_ field $ \row -> do
      drawStringHere w [borderChar]
      forM_ row $ \cell ->
        drawStringHere w [cellToChar cell]
      drawStringHere w  [borderChar, '\n']
    drawStringHere w borderLine
    drawStringHere w rules
  where
    width = V.length $ field ! 0
    borderChar = '^'
    borderLine = replicate (width + 2) borderChar ++ "\n"
    enumerate v = zip [0..] (V.toList v)
    rules = " q - exit\n\
            \ arrows - move"

startPos = (0, 0)

showDialog :: Window -> String -> [(String, Curses a)] -> Maybe (Curses a)
showDialog w title [] = Nothing
showDialog w title items = Just $ do
    updateWindow w clear
    moveCursorTo w (0, 0)
    drawStringHere w borderLine
    drawLine title
    drawStringHere w borderLine
    forM_ items $ \item ->
      drawLine (fst item)
    drawStringHere w borderLine
    drawStringHere w rules
    render
    num <- execStateT chooseOption 0
    snd $ items !! num
  where
    width = 20
    borderChar = '#'
    borderLine = replicate width borderChar ++ "\n"
    rules = " y - enter\n\
            \ arrows - select"

    drawLine s = drawStringHere w $
      [borderChar] ++ " " ++ s ++ replicate (width - length s - 3) ' ' ++ [borderChar, '\n']

    chooseOption :: StateT Int Curses ()
    chooseOption = do
      pos <- get
      lift $ moveCursorTo w (pos + 3, 2)
      lift render
      finished <- nextKey
      unless finished chooseOption

    lastItemNum = length items - 1
    nextKey :: StateT Int Curses Bool
    nextKey = do
      pos <- get
      mbEvent <- do
        w <- lift defaultWindow
        lift $ getEvent w Nothing
      case mbEvent of
        Just event -> case event of
          EventSpecialKey KeyUpArrow -> do
            put $ if pos == 0 then 0 else pos - 1
            return False
          EventSpecialKey KeyDownArrow -> do
            put $ if pos == lastItemNum then lastItemNum else pos + 1
            return False
          EventCharacter 'y' ->
            return True
          _ -> nextKey
        Nothing -> nextKey

showMainMenu w title = fromJust $ showDialog w title [("Play", showGame w), ("Exit", return ())]

showGame :: Window -> Curses ()
showGame w = do
  state <- runCursesUI $ execGame playGame field startPos
  showMainMenu w $ show $ gsStatus state

playWithGraphics :: IO ()
playWithGraphics = runCurses $ do
  setEcho False
  w <- defaultWindow
  showMainMenu w "Hello"

playWithText :: IO ()
playWithText = do
  state <- runTextUI $ execGame playGame field startPos
  print $ gsStatus state

main :: IO ()
main = do
  putStr "Do you want to enter graphic mode? (y / n) : "
  hFlush stdout
  ch <- getChar
  getChar
  case ch of
    'y' -> playWithGraphics
    'n' -> playWithText
    'q' -> return ()
    _ -> main
