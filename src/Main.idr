module Main

import Control.App
import Control.App.Console
import System

main : IO ()
main = do args <- getArgs
          case args of
            [] => pure ()
            (_ :: xs) => case xs of
                              [] => (putStrLn "Missing arguments")
                              _ => putStrLn <| show xs
-- (putStrLn (show xs))
          -- if length args > 1 then (let (_ :: args) = args in  putStrLn <| show args) else putStrLn "missing arguments"
