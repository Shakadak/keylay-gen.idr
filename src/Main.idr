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

randGene : HasIO io => io Char
randGene = rndSelect genePool

genMember : HasIO io => Nat -> io String
genMember n = map pack <| sequence <| replicate n <| randGene


genPop : HasIO io => Nat -> Nat -> io <| List String
genPop n s = sequence <| replicate n <| genMember s

sortByM : Monad m => Ord o => (a -> m o) -> List a -> m (List a)
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

mutate : HasIO io => String -> io String
mutate str = do
  let max : Int32 = cast (length str) - 1
  needle <- map cast <| randomRIO (0, max)
  char <- randGene
  let unpkd = fastUnpack str
      new = case inBounds needle unpkd of
        Yes prf => replaceAt needle char unpkd
        No _ => unpkd
  pure <| fastPack new

iterPop : HasIO io => Eq val => Show val
  => io val
  -> (List val -> io (List val))
  -> (val -> val -> io (List val))
  -> (List val -> io ())
  -> List val
  -> io (List val)
iterPop generate rank combine inspect oldGen = do
  nextGen <- sequence <| zipWith combine oldGen (fromMaybe [] <| tail' oldGen)
  pop <- rank <| nub (oldGen ++ concat nextGen)
  let pop = take (length oldGen) pop
  inspect pop
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
      let genMember = genMember <| length cfg.target
          rank = sortByM <| compute cfg.target
          inspect = \pop => putStrLn "Intermediate population: \{show pop}"
          combine = (>>= traverse mutate) .: combine
      pop <- nTimes cfg.iterations (>>= primIO . iterPop genMember rank combine inspect) <| pure initPop
      putStrLn "Final population:        \{show pop}"

main : IO ()
main = run program
