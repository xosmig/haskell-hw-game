
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( toField  -- FIXME: have to export to be able to test
    , GameState(..)  -- FIXME: have to export to be able to test
    , Direction(..)
    , GameUI(..)
    , GameT(..)
    , GameStatus(..)
    , playGame
    , runGameT
    , execGameT
    , evalGameT
    , gameState
    -- , move
    -- , isWin
    ) where


import Control.Monad.Trans.Class
-- import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.State
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

data GameState = GameState { gsPos :: Position, gsField :: Field, gsStatus :: GameStatus }
  deriving Show

gameState :: String -> Position -> Maybe GameState
gameState fieldS pos = (\field -> GameState pos field InGame) <$> toField fieldS

data Direction = North | South | West | East
  deriving (Eq, Show)

newtype GameT ui a = GameT (StateT GameState ui a)
  deriving (Functor, Applicative, Monad, MonadState GameState, Alternative)

runGameT :: GameT ui a -> GameState -> ui (a, GameState)
runGameT (GameT st) = runStateT st

execGameT :: GameUI ui => GameT ui a -> GameState -> ui GameState
execGameT game initial = snd <$> runGameT game initial

evalGameT :: GameUI ui => GameT ui a -> GameState -> ui a
evalGameT game initial = fst <$> runGameT game initial

instance MonadTrans GameT where
  lift f = GameT $ lift f

class Monad ui => GameUI ui where
  nextStep :: ui (Maybe Direction)
  movePlayer :: Position -> Position -> ui ()

-- instance GameUI ui => MonadPlus (GameT ui) where
--   mzero = do
--     lift killCharacter
--     GameT mzero
--   (GameT ma) `mplus` (GameT mb) = GameT $ ma `mplus` mb

evalStep :: GameUI ui => GameT ui ()
evalStep = do
    mbDir <- lift nextStep
    gs <- get
    case mbDir of
      Nothing -> put $ gs { gsStatus = Stop }
      Just dir -> do
        let oldPos = gsPos gs
        let newPos@(x, y) = go oldPos dir
        let field = gsField gs
        if x >= 0 && y >= 0 && V.length field /= 0
          && x < V.length field && y < V.length (field ! x)
        then do
          let cell = field ! x ! y
          when (cell /= Wall) $ do
            lift $ movePlayer oldPos newPos
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

playGame :: GameUI ui => GameT ui ()
playGame = do
  evalStep
  status <- gets gsStatus
  when (status == InGame)
    playGame
