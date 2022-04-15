module Main where

import Control.Concurrent (threadDelay)
import Process
import Channel
import PiQQ
-- import PrivateMVar
-- import GlobalMVar
-- import PrivateTMVar
-- import GlobalTMVar
import Async

main :: IO ()
main = do
  runProcess $
    -- new $ \chan ->
      -- keepSendingMsgs chan z `par` (repl $ recv chan (\y -> exec (act2 y) inert))
    
    -- new $ \x ->
    --   new $ \y ->
    --     send x "x" (exec (putStrLn "message sent on x") inert) `par`
    --     send y "y" (exec (putStrLn "message sent on y") inert) `par`
    --     choose x (\ch -> exec (putStrLn $ "message received on " ++ ch) stop)
    --             y (\ch -> exec (putStrLn $ "message received on " ++ ch) stop)

    -- new \chan ->
    --   asyncMultiSend chan 10 `par` asyncMultiRecv chan 5

    [pico| new x. x<z>. exec act1. 0 | x(y). exec act2 y.stop |]
  
  where
    z :: Int
    z = 5
    keepSendingMsgs chan msg = send chan msg (exec (act1 >> threadDelay 1000000) (keepSendingMsgs chan msg)) 
    act1 = putStrLn "message sent"
    act2 y = putStrLn $ "message received: " ++ show y
    
    asyncMultiSend :: AsyncChannel String -> Int -> Process
    asyncMultiSend chan counter
      | counter == 0 = inert
      | otherwise = send chan (show counter) (exec (putStrLn $ show counter ++ " sent") (asyncMultiSend chan (counter-1)))

    asyncMultiRecv :: AsyncChannel String -> Int -> Process
    asyncMultiRecv chan counter
      | counter == 0 = inert
      | otherwise = recv chan (\msg -> exec (putStrLn $ msg ++ " received") (asyncMultiRecv chan (counter-1)))