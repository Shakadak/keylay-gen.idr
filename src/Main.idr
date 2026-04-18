module Main

import Control.App
import Control.App.Console
import Data.List
import Data.Maybe
import Data.Monoid.Exponentiation
import System
import System.Random
import Text.Distance.Levenshtein

import Cli
import Solution

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

iterPop : Monad m => Eq val => Show val
  => (List val -> m (List val)) -- rank
  -> (val -> val -> m (List val)) -- combine
  -> (List val -> m ()) -- inspect
  -> List val
  -> m (List val)
iterPop rank combine inspect oldGen = do
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
      let genPop : HasIO io => Nat -> io $ List $ Guide 28
          genPop n = sequence $ replicate n newGuide
      initPop <- primIO $ genPop cfg.population
      putStrLn "Initial population:      \{show $ map (solution solutionMap) initPop}"
      let eval = (map (^2) . compute cfg.target)
          rank = sortByM $ eval . (solution solutionMap)
          combine = \left, right => do
            (left, right) <- if !(randomRIO (0.0, 1.0)) > 0.1 then crossover left right else pure (left, right)
            -- (left, right) <- crossover a b
            left <- if !(randomRIO (0.0, 1.0)) > 0.1 then mutate left else pure left
            -- left <- mutate left
            right <- if !(randomRIO (0.0, 1.0)) > 0.1 then mutate right else pure right
            -- right <- mutate right
            pure [left, right]
          -- inspect : HasIO io => List (Guide 28) -> io ()
          inspect = \pop => putStrLn "Target: \{cfg.target}; Intermediate top member: \{maybe "" (solution solutionMap) <| head' pop}"
      let -- iterator : HasIO io => io (List (Guide 28)) -> io (List (Guide 28))
          iterator = \mpop => mpop >>= (iterPop rank combine inspect)
      pop <- primIO $ nTimes cfg.iterations iterator <| pure initPop
      putStrLn "Final population:        \{show $ map (solution solutionMap) pop}"

main : IO ()
main = run program
