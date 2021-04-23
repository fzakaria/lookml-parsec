{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
    
import Control.Applicative ()
import Control.Arrow ()
import Control.Monad (when)
import System.Console.GetOpt
  ( ArgDescr (NoArg),
    ArgOrder (Permute),
    OptDescr (..),
    getOpt,
    usageInfo,
  )
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)
import Data.Text.IO (getContents)
import Text.Megaparsec as Parsec (errorBundlePretty)

import Parser (parse, pretty)
import qualified Text.PrettyPrint as Pretty

data Flag
  = Debug --  -d
  deriving (Eq, Ord, Enum, Show, Bounded)

flags :: [OptDescr (IO Flag)]
flags =
  [ Option
      ['d']
      ["debug"]
      ( NoArg
          (return Debug)
      )
      "Prints some helpful debugging information",
    Option
      ['h']
      ["help"]
      ( NoArg
          ( do
              prg <- getProgName
              hPutStrLn stderr (usageInfo prg flags)
              exitSuccess
          )
      )
      "Print this help message"
  ]

-- Using the GetOpt module from Haskell.
-- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
parseArgs :: IO [Flag]
parseArgs = do
  argv <- getArgs
  progName <- getProgName
  case getOpt Permute flags argv of
    (opts, [], []) -> sequence opts
    (_, _, errs) -> ioError (userError (concat errs ++ helpMessage))
      where
        header = "Usage: " ++ progName ++ " [OPTION...]"
        helpMessage = usageInfo header flags

-- the interact method takes String from IO and writes String to IO
main :: IO ()
main = do
  args <- parseArgs
  input <- Data.Text.IO.getContents
  let debug = Debug `elem` args
  let result = parse input
  let value = case result of
        (Left err) -> error $ Parsec.errorBundlePretty err
        (Right v) -> v
  when debug (print value)
  putStr $ (Pretty.render . pretty) value
  