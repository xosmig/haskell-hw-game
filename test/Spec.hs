

import Control.Exception (Exception, catch, throw)
import Control.Monad.Trans.Maybe
import Data.Maybe
import Lib
import System.Console.ANSI
import Control.Monad.Trans.State
import Control.Monad.Identity

-- instance GameUI FiniteSupply where
  -- nextEvent :: ui Direction

  -- movePlayer :: Position -> Position -> ui ()
  -- finish :: ui ()
  -- killPlayer :: ui ()



fieldExample =
  " #      \n\
  \   ###  \n\
  \   # x  \n\
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

--
-- stateExample :: GameState
-- stateExample = GameState (0, 0) (fromJust $ toField fieldExample)
--
--
-- testsMove = [ isNothing (run $ move West)
--             , isNothing (run $ move North)
--             , inPos (run $ move East) (0, 0)
--             , inPos (run $ move South) (1, 0)
--             , inPos (run to11) (1, 1)
--             ]
--   where
--     run game = runIdentity (runMaybeT (execStateT game stateExample))
--     inPos mbState pos = case mbState of
--         Just (GameState p field) -> p == pos
--         Nothing -> False
--     to11 = do
--       move South
--       move East
--
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
            -- , ("move", testsMove)
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


-- =================================================================

data Supply t a = Supply { splState :: State ([t], Integer) a }

runSupply :: Supply t a -> [t] -> (a, [t])
runSupply spl ts = let (a, (ts1, cnt)) = runState (splState spl) (ts, 0)
                    in (a, ts1)

evalSupply :: Supply t a -> [t] -> a
evalSupply spl ts = fst $ runSupply spl ts

supply :: Supply t (Maybe t)
supply = Supply $ state f
  where
    f (t:ts, i) = (Just t,  (ts, i + 1))
    f ([], i)     = (Nothing, ([], i))

supplyExample :: Supply Int (Maybe (Int, Int))
supplyExample = do
                  mi1 <- supply
                  mi2 <- supply
                  return $ (,) <$> mi1 <*> mi2

instance Functor (Supply t) where
  fmap f spl = Supply $ fmap f (splState spl)

instance Applicative (Supply t) where
  pure x = Supply $ pure x
  spl1 <*> spl2 = Supply $ splState spl1 <*> splState spl2

instance Monad (Supply t) where
  spl >>= k = Supply $ splState spl >>= splState . k

data FiniteSupply t a = FSup { getSupply :: Supply t (Maybe a) }

finSup :: FiniteSupply t t
finSup = FSup supply

runFinSup :: FiniteSupply t a -> [t] -> (Maybe a, [t])
runFinSup (FSup spl) = runSupply spl

evalFinSup :: FiniteSupply t a -> [t] -> Maybe a
evalFinSup (FSup spl) = evalSupply spl

instance Functor (FiniteSupply t) where
  f `fmap` (FSup spl) = FSup $ (f <$>) <$> spl

instance Applicative (FiniteSupply t) where
  pure = FSup . pure . Just
  (FSup spl1) <*> (FSup spl2) = FSup $ (<*>) <$> spl1 <*> spl2

instance Monad (FiniteSupply t) where
  (FSup spl) >>= k = FSup $ do
    mbVal <- spl
    case mbVal of
      (Just val)  -> getSupply (k val)
      Nothing     -> return Nothing

