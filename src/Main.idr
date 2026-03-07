module Main

import Control.App
import Control.App.Console
import Data.String
import System
import System.Console.GetOpt

nTimes : Nat -> (a -> a) -> a -> a
nTimes 0 f x = x
nTimes (S 0) f x = f x
nTimes (S k) f x = nTimes k f (f x)

record Config where
  constructor MkConfig
  iterations : Nat
  target : String
  verbose : Bool

data Flag
  = Iterations Nat
  | Target String
  | Verbose

flagSpecs : List (OptDescr Flag)
flagSpecs =
  [ MkOpt ['v'] ["verbose"] (NoArg Verbose) "Enable verbose output."
  ]

usage = usageInfo "Usage: keylay-gen [OPTIONS]" flagSpecs

handleNonOptions : List String -> List String
handleNonOptions [] = []
-- handleNonOptions [_] = []
handleNonOptions args = ["unexpected args: \{unwords args}"]

parseArgs : List String -> Either String Config
parseArgs strs =
  let res = getOpt RequireOrder flagSpecs strs
      errs = res.errors
        ++ map (\u => "unrecognized option \{u}") res.unrecognized
        ++ handleNonOptions res.nonOptions
  in case errs of
          [] => Right ?a_1
          (_ :: _) => Left <| unlines (errs ++ ["", usage])

skimProgPath : List String -> List String
skimProgPath [] = []
skimProgPath (_ :: xs) = xs

program : Has [Console, PrimIO] es => App es ()
program =
  do args <- primIO getArgs
     case parseArgs <| skimProgPath args of
       (Left error) => putStr error
       (Right x) => ?a_2

main : IO ()
main = run program
