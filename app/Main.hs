module Main where

--import Lib

import CartPole


main :: IO ()
main = do
  x <- initCPIO
  let
    ts = runCPEpisode x (\_ -> PushLeft)
  putStrLn $ show ts
  
