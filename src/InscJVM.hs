import CompileJVM

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Data.List.Split
import Data.List
import System.Process

import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant

import ErrM

type ParseFun a = [Token] -> Err a

run :: ParseFun Program -> String -> IO ()
run p f = do
  s <- readFile f
  let ts = myLexer s in
    case p ts of
      Bad s -> do
        putStrLn "\nParse              Failed...\n"
        putStrLn "Tokens:"
        putStrLn $ show ts
        putStrLn s
        exitFailure
      Ok tree -> do
        let (abspath, filename) = splitPath f
        writeFile (abspath ++ ".j") $ transProg filename tree
        ph <- runCommand $ "java -jar lib/jasmin.jar " ++ abspath ++ ".j;mv " ++
                            filename ++ ".class " ++ abspath ++ ".class"
        waitForProcess ph
        putStr (f ++ " successfully compiled\n")

splitPath :: String -> (String, String)
splitPath filepath =
  let
    path = splitOn "/" filepath
    filename = head (splitOn "." $ head $ reverse $ path)
    abspath = if (length path == 1) then filename
                else (intercalate "/" (take (length path - 1) path)) ++ "/" ++ filename
  in (abspath, filename)

main :: IO ()
main = do
  files <- getArgs
  case files of
    [file] -> run pProgram file
