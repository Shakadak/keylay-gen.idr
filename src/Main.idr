module Main

import Control.App
import Control.App.Console
import System
import System.Random
import Data.List

import Cli

nTimes : Nat -> (a -> a) -> a -> a
nTimes 0 f x = x
nTimes (S 0) f x = f x
nTimes (S k) f x = nTimes k f (f x)

selectSplit : List a -> (a, List a)
selectSplit [] = ?selectSplit_rhs_0
selectSplit (x :: xs) = ?selectSplit_rhs_1

genePool : List Char
genePool = unpack "abcdefghijklmnopqrstuvwxyz"

goGenMember : HasIO io => Nat -> io (List Char)
goGenMember 0 = pure []
goGenMember (S k) = [| rndSelect genePool :: goGenMember k |]

genMember : HasIO io => Nat -> io String
genMember n = map pack (goGenMember n)


genPop : HasIO io => Nat -> Nat -> io <| List String
genPop 0 _ = pure []
genPop (S k) s = [| genMember s :: genPop k s |]

iterPop : HasIO io => List String -> io (List String)
iterPop strs =
  do pop <- traverse (genMember . String.length) strs
     putStrLn "Intermediate population: \{show pop}"
     pure pop

program : Has [Console, PrimIO] es => App es ()
program =
  do args <- primIO getArgs
     case parseArgs args of
       Left error => putStr error
       Right cfg =>
        do
          putStrLn """
          target = \{cfg.target}
          iterations = \{show cfg.iterations}
          population = \{show cfg.population}
          verbose = \{show cfg.verbose}
          """
          initPop <- primIO <| genPop cfg.population <| length cfg.target
          putStrLn "Initial population:      \{show initPop}"
          pop <- nTimes cfg.iterations (>>= primIO . iterPop) <| pure initPop
          putStrLn "Final population:        \{show pop}"

main : IO ()
main = run program
