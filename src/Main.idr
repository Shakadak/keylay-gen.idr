module Main

import Control.App
import Control.App.Console

hello : Console es => App es ()
hello = (putStrLn "Hello, App world!")

data Counter : Type where

helloCount : (Console es, (State Counter Int es)) => App es ()
helloCount = do c <- get Counter
                put Counter (c + 1)
                putStrLn "Hello, counting world"
                c <- get Counter
                putStrLn ("Counter " ++ show c)

main : IO ()
main = run (new 93 helloCount)
