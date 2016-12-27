
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( toField  -- FIXME: have to export to be able to test
    , GameState(..)  -- FIXME: have to export to be able to test
    , Direction(..)
    , GameUI
    , GameT(..)
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


data NotImplemented = NotImplemented
instance Show NotImplemented where
  show NotImplemented = "Not Implemented"
instance Exception NotImplemented where
(??) = throw NotImplemented

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

data GameState = GameState { gsPos :: Position, gsField :: Field }
  deriving Show

data Direction = North | South | West | East
  deriving Show

newtype GameT ui a = GameT (StateT GameState (MaybeT ui) a)
  deriving (Functor, Applicative, Monad, MonadState GameState, Alternative)

instance MonadTrans GameT where
  lift f = GameT (lift $ lift f)

class Monad ui => GameUI ui where
  nextEvent :: ui Direction
  movePlayer :: Position -> Position -> ui ()
  finish :: ui ()
  killPlayer :: ui ()

instance GameUI ui => MonadPlus (GameT ui) where
  mzero = do
    lift killPlayer
    GameT mzero
  (GameT ma) `mplus` (GameT mb) = GameT $ ma `mplus` mb

makeStep :: GameUI ui => GameT ui ()
makeStep = do
    dir <- lift nextEvent
    gs <- get
    let newPos@(x, y) = go (gsPos gs) dir
    let field = gsField gs
    guard $ x >= 0 && y >= 0 && V.length field /= 0
      && x < V.length field && y < V.length (field ! x)
    case field ! x ! y of
      Exit -> lift finish
      Free -> put $ gs { gsPos = newPos }
      Wall -> return ()
  where
    go (x, y) dir = case dir of
      South -> (x + 1, y)
      North -> (x - 1, y)
      West  -> (x, y - 1)
      East  -> (x, y + 1)


playGame :: GameUI ui => GameT ui b
playGame = forever makeStep