
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Exception (Exception, catch, throw)
import Control.Monad.Trans.Maybe
import Data.Maybe
import Lib
import System.Console.ANSI
-- import Control.Monad.Trans.State
import Control.Monad.Identity
import Control.Monad.State


newtype FakeUI a = FakeUI (State [Direction] a)
  deriving (Functor, Applicative, Monad, MonadState [Direction])

evalFakeUI :: FakeUI a -> [Direction] -> a
evalFakeUI (FakeUI st) = evalState st

instance GameUI FakeUI where
  -- nextStep :: ui Direction
  nextStep = do
    ds <- get
    case ds of
      (dir : rest) -> do
        put rest
        return $ Just dir
      [] -> return Nothing
  -- movePlayer :: Position -> Position -> ui ()
  movePlayer _ _ = return ()

fieldExample =
  " #x     \n\
  \   ###  \n\
  \   #    \n\
  \  ###   \n\
  \        "

fieldNotRect =
  "    #   \n\
  \   ### \n\
  \   # x  \n\
  \  ###   \n\
  \        "

fieldWoExit =
  "    #   \n\
  \   ###  \n\
  \   #    \n\
  \  ###   \n\
  \        "

fieldTwoExits =
  "    #  x\n\
  \   ###  \n\
  \   # x  \n\
  \  ###   \n\
  \        "

testsField =  [ isJust    (toField fieldExample)
              , isNothing (toField fieldNotRect)
              , isNothing (toField fieldWoExit)
              , isNothing (toField fieldTwoExits)
              ]

stateExample :: GameState
stateExample = fromJust $ gameState fieldExample (0, 0)

testsPlay = [ gsStatus (run [North]) == Lose
            , gsStatus (run [West]) == Lose
            , gsStatus (run [South, East, East, North]) == Win
            , gsPos (run [East]) == (0, 0)
            , gsPos (run [South, South]) == (2, 0)
            ]
  where
    run ds = evalFakeUI (execGameT playGame stateExample) ds


-- =================================================================

data Skipped = Skipped
instance Show Skipped where
  show Skipped = "skipped"
instance Exception Skipped where

(??) = throw Skipped

printColored color msg = do
  setSGR [SetColor Foreground Vivid color]
  putStrLn msg
  setSGR [Reset]

testAll :: IO ()
testAll = do
    putStrLn ""
    mapM_ (uncurry runTests) tests
  where
    tests = [ ("field", testsField)
            , ("play", testsPlay)
            ]

    runTests :: String -> [Bool] -> IO ()
    runTests n ts = do
      putStr $ "Running tests " ++ show n ++ "... "
      uncurry printColored $
        if and ts then
          (Green, "OK")
        else
          (Red, "FAIL")
      `catch` \e -> case e of
        Skipped -> printColored Yellow $ show e

main :: IO ()
main = testAll
