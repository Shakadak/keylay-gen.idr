module Main

import Control.App
import Control.App.Console
import Data.Maybe
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
  population : Nat
  target : String
  verbose : Bool

data Flag
  = Iterations Nat
  | Population Nat
  | Target String
  | Verbose
  | Help

Eq Flag where
  Iterations x == Iterations y = x == y
  Population x == Population y = x == y
  Target x == Target y = x == y
  Verbose == Verbose = True
  Help == Help = True
  _ == _ = False

defaultConfig : Config
defaultConfig = MkConfig 0 0 "" False

parseIterations : String -> Either String Flag
parseIterations str =
  case parsePositive str of
    Nothing => Left ("Invalid value for --count: \{str}")
    (Just n) => Right (Iterations n)

flagSpecs : List (OptDescr Flag)
flagSpecs =
  [ MkOpt ['v'] ["verbose"] (NoArg Verbose) "Enable verbose output."
  , MkOpt ['h'] ["help"] (NoArg Help) "Show this help."
  , MkOpt ['i'] ["iterations"] (ReqArg' parseIterations "N") "How many iterations to run."
  , MkOpt ['p'] ["population"] (ReqArg' parseIterations "P") "How large the population is."
  ]

usage = usageInfo "Usage: keylay-gen [OPTIONS]" flagSpecs

handleNonOptions : List String -> List String
handleNonOptions [] = []
-- handleNonOptions [_] = []
handleNonOptions args = ["unexpected args: \{unwords args}"]

applyFlag : Flag -> Config -> Config
applyFlag (Iterations n) cfg = { iterations := n } cfg
applyFlag (Population p) cfg = { population := p } cfg
applyFlag (Target str) cfg = { target := str } cfg
applyFlag Verbose cfg = { verbose := True } cfg
applyFlag Help cfg = cfg

wantsHelp = isJust . Data.List.find (== Help)

parseArgs : List String -> Either String Config
parseArgs strs =
  let res = getOpt RequireOrder flagSpecs strs
      errs = res.errors
        ++ map (\u => "unrecognized option \{u}") res.unrecognized
        ++ handleNonOptions res.nonOptions
  in case errs of
          (_ :: _) => Left <| unlines (errs ++ ["", usage])
          [] =>
            if wantsHelp res.options
            then Left usage
            else Right (foldl (flip applyFlag) defaultConfig res.options)

skimProgPath : List String -> List String
skimProgPath [] = []
skimProgPath (_ :: xs) = xs

program : Has [Console, PrimIO] es => App es ()
program =
  do args <- primIO getArgs
     case parseArgs <| skimProgPath args of
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
