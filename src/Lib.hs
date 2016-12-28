
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( toField
    , Field
    , GameState
    , gsStatus
    , Direction(..)
    , GameUI(..)
    , GameStatus(..)
    , playGame
    , runGame
    , execGame
    , evalGame
    , cellToChar
    ) where


import Control.Monad.Trans.Class
-- import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad
import Control.Exception
import qualified Data.Vector as V
import Data.Vector((!))
import Control.Applicative


-- FIXME: ??? data Cell = Exit('x'), Wall('#'), Free(' ')
data Cell = Exit | Wall | Free
  deriving (Show, Eq)

toCell :: Char -> Maybe Cell
toCell c = case c of
  'x' -> pure Exit
  '#' -> pure Wall
  ' ' -> pure Free
  _   -> mzero

cellToChar :: Cell -> Char
cellToChar cell = case cell of
  Exit -> 'x'
  Wall -> '#'
  Free -> ' '

type Position = (Int, Int)
type Vector2D a = V.Vector (V.Vector a)
type Field = Vector2D Cell

-- Функция проверяет, что поле имеет прямоугольную форму,
-- состоит из понятных символов и на нём ровно одна цель.
toField :: String -> Maybe Field
toField s = do
    guard $ length (filter (== Just Exit) (toCell <$> s)) == 1
    field <- unchecked
    let width = V.length $ field ! 0
    guard $ all (\row -> V.length row == width) field
    return field
  where
    unchecked = sequence $ V.fromList $ (sequence . (toCell <$>) . V.init . V.fromList) <$> lines s

data GameStatus = Win | InGame | Lose | Stop
  deriving (Eq, Show)

data GameState = GameState { gsPos :: Position, gsStatus :: GameStatus }
  deriving Show

data Direction = North | South | West | East
  deriving (Eq, Show)

newtype Game ui a = Game (ReaderT Field (StateT GameState ui) a)
  deriving (Functor, Applicative, Monad, MonadState GameState, MonadReader Field)

runGame :: Game ui a -> Field -> Position -> ui (a, GameState)
runGame (Game game) field pos = runStateT (runReaderT game field) initState
  where
    initState = GameState { gsPos = pos, gsStatus = InGame }

execGame :: GameUI ui => Game ui a -> Field -> Position -> ui GameState
execGame game field pos = snd <$> runGame game field pos

evalGame :: GameUI ui => Game ui a -> Field -> Position -> ui a
evalGame game field pos = fst <$> runGame game field pos

instance MonadTrans Game where
  lift f = Game $ lift $ lift f

class Monad ui => GameUI ui where
  nextStep :: ui (Maybe Direction)
  movePlayer :: Position -> ui ()

-- instance GameUI ui => MonadPlus (Game ui) where
--   mzero = do
--     lift killCharacter
--     Game mzero
--   (Game ma) `mplus` (Game mb) = Game $ ma `mplus` mb

evalStep :: GameUI ui => Game ui ()
evalStep = do
    mbDir <- lift nextStep
    gs <- get
    case mbDir of
      Nothing -> put $ gs { gsStatus = Stop }
      Just dir -> do
        let newPos@(x, y) = go (gsPos gs) dir
        field <- ask
        if x >= 0 && y >= 0 && V.length field /= 0
          && x < V.length field && y < V.length (field ! x)
        then do
          let cell = field ! x ! y
          when (cell /= Wall) $ do
            lift $ movePlayer newPos
            put $ gs { gsPos = newPos }
          when (cell == Exit) $
            put $ gs { gsStatus = Win }
        else
          put $ gs { gsStatus = Lose }
  where
    go (x, y) dir = case dir of
      South -> (x + 1, y)
      North -> (x - 1, y)
      West  -> (x, y - 1)
      East  -> (x, y + 1)

playGame :: GameUI ui => Game ui ()
playGame = do
  evalStep
  status <- gets gsStatus
  when (status == InGame)
    playGame
