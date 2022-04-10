module Main where

-- import PrivateMVar
import GlobalMVar

main :: IO ()
main = do
  runProcess $ 
    new $ \x -> 
      output x z (exec act1 inert) `par` input x (\y -> exec (act2 y) stop)
  where
    z :: Int
    z = 5
    act1 = putStrLn "message sent"
    act2 y = putStrLn $ "message received: " ++ show y 
