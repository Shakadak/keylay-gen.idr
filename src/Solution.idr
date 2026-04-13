module Solution

import Data.Vect

public export
record Solution where
  constructor MkSolution
  left : Vect 26 Nat

data Map : Nat -> Type where
  MkMap : Vect n Char -> Map n

data Dominance = MkDominance Nat

data Guide : Nat -> Type where
  MkGuide : Vect (2 * n) (Fin n, Dominance) -> Guide n

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

solution : {n : Nat} -> Map n -> Guide n -> String
solution (MkMap xs) (MkGuide guide) = pack $ toList $ updateMap (map extractSwap $ pairings guide) xs
