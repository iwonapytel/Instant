module CompileLLVM where

import Data.Set as S
import Control.Monad.State
import AbsInstant
import Data.List
import Data.List.Split

type VState = State (Set String, Integer)

initialState = (S.empty, 1)

transExp :: Exp -> VState (String, String)
transExp (ExpLit lit) =
  return (show lit, "")

transExp (ExpVar (Ident ident)) = do
  (vars, nextReg) <- get
  case S.member ident vars of
    True -> do
      put (vars, nextReg + 1)
      return ("%" ++ show nextReg, "%" ++ show nextReg ++ " = load i32, i32* %" ++ ident ++ "\n")
    False -> error "Non-existing identifier"

transExp (ExpAdd exp1 exp2) =
  transExpBin exp1 exp2 " = add nsw i32 "

transExp (ExpSub exp1 exp2) =
  transExpBin exp1 exp2 " = sub nsw i32 "

transExp (ExpMul exp1 exp2) =
  transExpBin exp1 exp2 " = mul nsw i32 "

transExp (ExpDiv exp1 exp2) =
  transExpBin exp1 exp2 " = sdiv i32 "

transExpBin :: Exp -> Exp -> String -> VState (String, String)
transExpBin exp1 exp2 op = do
  (r1, str1) <- transExp exp1
  (r2, str2) <- transExp exp2
  (vars, nextReg) <- get
  put (vars, (nextReg + 1))
  return ("%" ++ show nextReg, str1 ++ str2 ++ "%" ++ show nextReg ++ op ++ r1 ++
          ", "++ r2 ++ "\n")

transStmt :: Stmt -> VState String
transStmt (SExp expr) = do
  (reg, str) <- transExp expr
  return (str ++ "call void @printInt(i32 " ++ reg ++ ")\n")

transStmt (SAss (Ident ident) expr) = do
  (reg, str) <- transExp expr
  (vars, nextReg) <- get
  case S.member ident vars of
    True ->
      return (str ++ "store i32 " ++ reg ++ ", i32* %" ++ ident ++ " \n")
    False -> do
      put (S.insert ident vars, nextReg)
      return (str ++ "%" ++ ident ++ " = alloca i32\n" ++
              "store i32 " ++ reg ++ ", i32* %" ++ ident ++ " \n")

transStmts :: [Stmt] -> VState String
transStmts (x:xs) = do
  stmt1 <- transStmt x
  stmt2 <- transStmts xs
  return (stmt1 ++ stmt2)

transStmts [] = do
  return ""

transProg :: Program -> String
transProg (Prog prog) = do
  let (str, state) = runState (transStmts prog) initialState
  printDeclaration ++ prefix ++ str ++ suffix


prefix = "define i32 @main() {\n"
printDeclaration =  "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n\n" ++
                   "declare i32 @printf(i8*, ...)\n\n" ++
                   "define void @printInt(i32 %x) {\n" ++
                   "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n" ++
                   "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n" ++
                   "ret void\n" ++
                   "}\n\n"
suffix = "ret i32 0\n}"
