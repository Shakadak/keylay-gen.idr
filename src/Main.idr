module Main

import Control.App
import Control.App.Console
import System



main : IO ()
main = do args <- getArgs
          putStrLn <| show args
