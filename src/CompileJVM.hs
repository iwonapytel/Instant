module CompileJVM where

import Data.Map as M
import Control.Monad.State
import AbsInstant
import Data.List
import Data.List.Split

type VariablesMap = M.Map String Integer
type VState = State VariablesMap

counter = "$"
initialState :: VariablesMap
initialState = M.singleton counter 1

transExp :: Exp -> VState (Integer, String)
transExp (ExpLit lit) =
  return (1, litConst lit)

transExp (ExpVar (Ident ident)) = do
  vars <- get
  case M.lookup ident vars of
    Just num -> return (1, "iload " ++ show num ++ "\n")
    Nothing -> error "Non-existing identifier"

transExp (ExpAdd exp1 exp2) =
  transExpBin exp1 exp2 "iadd"

transExp (ExpSub exp1 exp2) =
  transExpBin exp1 exp2 "isub"

transExp (ExpMul exp1 exp2) =
  transExpBin exp1 exp2 "imul"

transExp (ExpDiv exp1 exp2) =
  transExpBin exp1 exp2 "idiv"

transExpBin :: Exp -> Exp -> String -> VState (Integer, String)
transExpBin exp1 exp2 binop = do
  (depth1, val1) <- transExp exp1
  (depth2, val2) <- transExp exp2
  if depth1 >= depth2 then
    return (1 + depth1, val1 ++ val2 ++ binop ++ "\n")
  else
    return (1 + depth2, val2 ++ val1 ++ "swap\n" ++ binop ++ "\n")

transStmts :: [Stmt] -> VState String
transStmts (x:xs) = do
  stmt1 <- transStmt x
  stmt2 <- transStmts xs
  return (stmt1 ++ stmt2)

transStmts [] = do
  return ""

transStmt :: Stmt -> VState String
transStmt (SExp expr) = do
  (_, str) <- transExp expr
  return (printVal str)

transStmt (SAss (Ident ident) expr) = do
  (_, str) <- transExp expr
  vars <- get
  case M.lookup ident vars of
    Just num -> return (str ++ "istore " ++ show num ++ "\n")
    Nothing -> do
      case M.lookup counter vars of
        Just nextVar -> do
          put (M.insert ident nextVar (M.insert counter (nextVar + 1) vars))
          return (str ++ "istore " ++ show nextVar ++ "\n")
        Nothing ->  error "Missing index counter"

transProg :: String -> Program -> String
transProg className (Prog prog) = do
  let (str, vars) = runState (transStmts prog) initialState
  (prefix className) ++ ".limit stack " ++ show (stackLimit str) ++ "\n" ++
          ".limit locals " ++ show (M.size vars) ++ "\n" ++
          str ++ suffix

litConst :: Integer -> String
litConst l =
  let str = if l < 6 then "iconst_" else "ldc "
  in str ++ show l ++ "\n"

prefix :: String -> String
prefix className = ".class public " ++ className ++ "\n" ++
        ".super java/lang/Object\n" ++
        ".method public <init>()V\n" ++
        ".limit stack 1\n" ++
        ".limit locals 1\n" ++
        ".line 1\n" ++
        "aload_0\n" ++
        "invokespecial java/lang/Object/<init>()V\n" ++
        "return\n" ++
        ".end method\n" ++
        "\n" ++
        ".method public static main([Ljava/lang/String;)V\n"

suffix = "return\n.end method"

printVal :: String -> String
printVal str =
  "getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
  str ++
  "invokevirtual java/io/PrintStream/println(I)V\n"

getStackSize :: [String] -> (Int, Int) -> (Int, Int)
getStackSize [] (acc, maxStack) = (acc, maxStack)

getStackSize (x:xs) (acc, maxStack) =
  let acc2 = if or (Prelude.map (\y -> isPrefixOf y x) ["getstatic", "iload", "iconst", "ldc"])
                then (acc + 1)
             else if isPrefixOf "invokevirtual java/io/PrintStream/println(I)V" x
                then (acc - 2)
             else if isPrefixOf "swap" x
               then acc
             else (acc - 1)
  in
    getStackSize xs (acc2, max acc2 maxStack)

stackLimit :: String -> Int
stackLimit str =
  let
    splitted = splitOn "\n" str
    (_, maxStack) = getStackSize splitted (0, 0)
  in
    maxStack
