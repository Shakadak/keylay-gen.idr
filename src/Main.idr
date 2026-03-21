module Main

import Control.App
import Control.App.Console
import System

import Cli

nTimes : Nat -> (a -> a) -> a -> a
nTimes 0 f x = x
nTimes (S 0) f x = f x
nTimes (S k) f x = nTimes k f (f x)


program : Has [Console, PrimIO] es => App es ()
program =
  do args <- primIO getArgs
     case parseArgs args of
       Left error => putStr error
       Right cfg =>
        putStrLn """
        target = \{cfg.target}
        iterations = \{show cfg.iterations}
        population = \{show cfg.population}
        verbose = \{show cfg.verbose}
        """

main : IO ()
main = run program
