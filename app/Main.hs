module Main where

import Control.Concurrent (threadDelay)
import Process
import Channel
import PiQQ
import PrivateMVar
import Examples (transmitList)
-- import GlobalMVar
-- import PrivateTMVar
-- import GlobalTMVar
-- import Async

main :: IO ()
main = do
  l <- runProcess $ transmitList [1..1000000]
  print $ length l