module Main

import Control.App
import Control.App.Console
import System

-- nTimes : Nat -> (a -> a) -> a -> a
-- nTimes 0 f = id
-- nTimes (S 0) f = f
-- nTimes (S k) f = f . nTimes k f

nTimes : Nat -> (a -> a) -> a -> a
nTimes 0 f x = x
nTimes (S 0) f x = f x
nTimes (S k) f x = nTimes k f (f x)

main : IO ()
main =
  do args <- getArgs
     case args of
       [] => pure ()
       (_ :: xs) => case xs of
         [] => (putStrLn "Missing arguments")
         [n] => (printLn <| (nTimes (cast n) (+ 1) 0))
         _ => putStrLn <| show xs
