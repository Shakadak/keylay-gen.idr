module Main

import Control.App
import Control.App.Console
import System
import System.Random
import Data.List
import Text.Distance.Levenshtein

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

genMember : HasIO io => Nat -> io String
genMember n = map pack <| sequence <| replicate n <| rndSelect genePool


genPop : HasIO io => Nat -> Nat -> io <| List String
genPop n s = sequence <| replicate n <| genMember s

sortByM : Monad m => Ord ord => (a -> m ord) -> List a -> m (List a)
sortByM by xs = do
  ts <- traverse (\x => (x,) <$> by x) xs
  pure <| map fst <| sortBy (compare `on` snd) ts

iterPop : HasIO io => Ord ord => (String -> io ord) -> List String -> io (List String)
iterPop evaluateMember strs = do
  pop <- traverse (genMember . String.length) strs
  pop <- sortByM evaluateMember (pop ++ strs)
  let pop = take (length strs) pop
  putStrLn "Intermediate population: \{show pop}"
  pure pop

program : Has [Console, PrimIO] es => App es ()
program = do
  args <- primIO getArgs
  case parseArgs args of
    Left error => putStr error
    Right cfg => do
      putStrLn """
      target = \{cfg.target}
      iterations = \{show cfg.iterations}
      population = \{show cfg.population}
      verbose = \{show cfg.verbose}
      """
      initPop <- primIO <| genPop cfg.population <| length cfg.target
      putStrLn "Initial population:      \{show initPop}"
      pop <- nTimes cfg.iterations (>>= primIO . iterPop (compute cfg.target)) <| pure initPop
      putStrLn "Final population:        \{show pop}"

main : IO ()
main = run program
