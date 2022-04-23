module Main where

import Control.Concurrent (threadDelay)
import Process
import Channel
import PiQQ
import PrivateMVar
-- import GlobalMVar
-- import PrivateTMVar
-- import GlobalTMVar
-- import Async

main :: IO ()
main = runProcess $
  new \sortedList -> new \quicksort ->
    (send quicksort ([3,9,5,1,7], sortedList) inert) `par`
    (recv sortedList \l -> exec (putStrLn $ "sorted list: " ++ show l) (stop ())) `par`
    (repl $ recv quicksort \(l, res) -> case l of
      [] -> send res [] inert
      [x] -> send res [x] inert
      (x:xs) ->
        new \res1 -> new \res2 ->
          let (less, greater) = partition (<x) xs
          in send quicksort (less, res1) $ 
              send quicksort (greater, res2) $
                recv res1 \l1 ->
                  recv res2 \l2 ->
                    send res (l1 ++ (x:l2)) inert
    )
    where
      partition :: Ord a => (a -> Bool) -> [a] -> ([a], [a])
      partition _ [] = ([], [])
      partition pred (x:xs)
        | pred x = (x:r1, r2)
        | otherwise = (r1, x:r2)
        where
          (r1, r2) = partition pred xs
  -- [pico| new x. (x<a+b>. exec act1. 0 | x(y). exec (putStrLn $ "message received: " ++ show y). 0) |]
  -- where
  --   act1= putStrLn "message sent"
  --   act2 x = putStrLn $ "message received: " ++ show x
  --   a = 3
  --   b = 7
--   runProcess $
--     -- new $ \chan ->
--       -- keepSendingMsgs chan z `par` (repl $ recv chan (\y -> exec (act2 y) inert))
    
--     -- new $ \x ->
--     --   new $ \y ->
--     --     send x "x" (exec (putStrLn "message sent on x") inert) `par`
--     --     send y "y" (exec (putStrLn "message sent on y") inert) `par`
--     --     choose x (\ch -> exec (putStrLn $ "message received on " ++ ch) stop)
--     --             y (\ch -> exec (putStrLn $ "message received on " ++ ch) stop)

--     -- new \chan ->
--     --   asyncMultiSend chan 10 `par` asyncMultiRecv chan 5

--   where
--     z :: Int
--     z = 5
--     keepSendingMsgs chan msg = send chan msg (exec (act1 >> threadDelay 1000000) (keepSendingMsgs chan msg)) 
--     act1 = putStrLn "message sent"
--     actA y = putStrLn $ "message received on channel a: " ++ show y
--     actB y = putStrLn $ "message received on channel b: " ++ show y
--     actC y = putStrLn $ "message received on channel c: " ++ show y
    
--     -- asyncMultiSend :: AsyncChannel String -> Int -> Process
--     -- asyncMultiSend chan counter
--     --   | counter == 0 = inert
--     --   | otherwise = send chan (show counter) (exec (putStrLn $ show counter ++ " sent") (asyncMultiSend chan (counter-1)))

--     -- asyncMultiRecv :: AsyncChannel String -> Int -> Process
--     -- asyncMultiRecv chan counter
--     --   | counter == 0 = inert
--     --   | otherwise = recv chan (\msg -> exec (putStrLn $ msg ++ " received") (asyncMultiRecv chan (counter-1)))