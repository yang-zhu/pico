module Sum(choose) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newEmptyTMVarIO)
import Environment (Environment(..))
import Process (Process(..))


choose :: Process a -> Process a -> Process a
choose (Proc p1) (Proc p2) = Proc \env@Env{belowSum} -> do
  progress <- maybe newEmptyTMVarIO return belowSum
  forkIO $ p2 env{belowSum = Just progress}
  p1 env{belowSum = Just progress}