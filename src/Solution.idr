module Solution

import Data.Vect
import Data.Nat
import System.Random


{- total
doubleMult : (n : Nat) -> 2 * n = n + n
doubleMult n = rewrite plusCommutative n 0 in Refl -}

data Map : Nat -> Type where
  MkMap : Vect (S n) Char -> Map (S n)

data Dominance = MkDominance Nat

public export
data Guide : Nat -> Type where
  MkGuide : Vect (2 * (S n)) (Fin (S n), Dominance) -> Guide (S n)

public export
record Solution where
  constructor MkSolution
  left : Guide 28

Show Dominance where
  show (MkDominance n) = show n

Eq Dominance where
  (MkDominance x) == (MkDominance y) = x == y

export
Show (Guide len) where
  show (MkGuide xs) = show xs

export
Eq (Guide len) where
  (MkGuide xs) == (MkGuide ys) = xs == ys

export
newGuide : HasIO io => {n : _} -> io $ Guide (S n)
newGuide =
  map MkGuide $ sequence $ replicate (2 * (S n)) $ map (, MkDominance 0) $ rndSelect' $ allFins (S n)

export
solutionMap : Map 28
solutionMap = MkMap $ fromList $ unpack "abcdefghijklmnopqrstuvwxyz**"

pairings : {n : Nat} -> Vect (2 * n) a -> Vect n (a, a)
pairings xs = map (\[a, b] => (a, b)) $ nSplits 2 n xs

swap : (Fin n, Fin n) -> Vect n a -> Vect n a
swap (x, y) xs = let
  a = index x xs
  b = index y xs
  in replaceAt x b $ replaceAt y a xs

updateMap : Vect _ (Fin n, Fin n) -> Vect n a -> Vect n a
updateMap xs ys = (foldl (flip swap) ys xs)

extractSwap : ((a, _), (b, _)) -> (a, b)
extractSwap ((a, _), (b, _)) = (a, b)

export
solution : {n : Nat} -> Map n -> Guide n -> String
solution (MkMap xs) (MkGuide guide) =
  pack $ toList $ updateMap (map extractSwap $ pairings {n = n} guide) xs

export
mutate : HasIO io => {n : _} -> Guide n -> io $ Guide n
mutate (MkGuide xs) = do
  target <- rndSelect' $ allFins (2 * n)
  value <- rndSelect' $ allFins n
  pure $ MkGuide $ replaceAt target (value, MkDominance 1) xs

export
crossover : HasIO io => {n : _} -> Guide n -> Guide n -> io $ (Guide n, Guide n)
crossover (MkGuide xs) (MkGuide ys) = do
  let left = MkGuide $ (take n xs) ++ (drop n ys)
  let right = MkGuide $ (take n ys) ++ (drop n xs)
  pure $ (left, right)
