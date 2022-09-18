module Sum(choose) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newEmptyTMVarIO)
import Environment (Environment(..))
import Process (Process(..))


-- | One of the two given processes executes and the other one is discarded.
-- It corresponds to the sum operator in the pi-calculus.
-- The operands have to be 'recv', 'inert' or 'choose'.
choose :: Process a -> Process a -> Process a
choose (Proc p1) (Proc p2) = Proc \env@Env{belowSum} -> do
  progress <- maybe newEmptyTMVarIO return belowSum
  forkIO $ p2 env{belowSum = Just progress}
  p1 env{belowSum = Just progress}