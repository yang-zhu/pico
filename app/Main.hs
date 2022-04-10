module Main where

import Control.Concurrent (threadDelay)
import PrivateMVar
-- import GlobalMVar

main :: IO ()
main = do
  runProcess $ 
    new $ \chan ->
      keepSendingMsgs chan z `par` (repl $ input chan (\y -> exec (act2 y) inert))
  where
    z :: Int
    z = 5
    keepSendingMsgs chan msg = output chan msg (exec (act1 >> threadDelay 1000000) (keepSendingMsgs chan msg)) 
    act1 = putStrLn "message sent"
    act2 y = putStrLn $ "message received: " ++ show y 
