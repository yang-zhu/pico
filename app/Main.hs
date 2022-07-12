module Main where

import Process
import Sum
import Channel
-- import PrivateMVar
-- import GlobalMVar
import PrivateTMVar
-- import GlobalTMVar
-- import Async
import Examples (transmitList)


main :: IO ()
main = do
  -- l <- runProcess $ transmitList [1..1000000]
  -- print $ length l
  runProcess $ new \chan ->
    send chan sent (exec (putStrLn ("sent: " ++ show sent)) inert) `par` 
    choose
      (recv chan (\received -> exec (putStrLn ("received + 1: " ++ show (received + 1))) (stop ())))
      (recv chan (\received -> exec (putStrLn ("received + 2: " ++ show (received + 2))) (stop ())))
  where
    sent = 1