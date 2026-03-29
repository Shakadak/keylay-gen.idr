module Main

import Control.App
import Control.App.Console
import Data.List
import Data.Maybe
import System
import System.Random
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

combine : HasIO io => String -> String -> io (List String)
combine l r = do
  let max : Int32 = cast (length l) - 1
  needle <- map cast <| randomRIO (0, max)
  let (hl, tl) = splitAt needle <| fastUnpack l
  let (hr, tr) = splitAt needle <| fastUnpack r
  pure <| map fastPack [hl ++ tr, hr ++ tl]

%ambiguity_depth 5
iterPop : HasIO io => Ord ord => (String -> io ord) -> (l : List String) -> io (List String)
iterPop evaluateMember oldGen = do
  newGen <- traverse (genMember . String.length) oldGen
  pop <- sortByM evaluateMember <| nub (newGen ++ oldGen)
  nextGen <- sequence <| zipWith combine pop (fromMaybe [] <| tail' pop)
  pop <- sortByM evaluateMember <| nub (pop ++ concat nextGen)
  let pop = take (length oldGen) pop
  putStrLn "Intermediate population: \{show pop}"
  pure pop
%ambiguity_depth 3

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
