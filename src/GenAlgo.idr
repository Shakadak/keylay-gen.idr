module GenAlgo

import Data.List
import Data.Maybe

export
record Callback where
  constructor MkCallback
  genPop : Monad m => Nat -> m (List a)
  inspect : Monad m => List a -> m ()
  combine : Monad m => a -> a -> m (List a)
  format : a -> String

export
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
