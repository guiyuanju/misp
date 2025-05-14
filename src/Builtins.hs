module Builtins
  ( builtins,
  )
where

import Data.List as L
import Data.Map as M
import Model

builtins =
  M.fromList
    [ ("+", Builtin "+" (numericBinaryOp (+) . checkBinary)),
      ("-", Builtin "-" (numericBinaryOp (-) . checkBinary)),
      ("*", Builtin "*" (numericBinaryOp (*) . checkBinary)),
      ("/", Builtin "/" (numericBinaryOp (/) . checkSndNotZero . checkBinary)),
      ("println", Builtin "println" (\args -> mispPrintMulti args >> pure (List []))),
      ("<", Builtin "<" (comparisonBinaryOp (<) . checkBinary)),
      ("<=", Builtin "<" (comparisonBinaryOp (<=) . checkBinary)),
      (">", Builtin "<" (comparisonBinaryOp (>) . checkBinary)),
      (">=", Builtin "<" (comparisonBinaryOp (>=) . checkBinary)),
      ("=", Builtin "<" (comparisonBinaryOp (==) . checkBinary)),
      ("and", Builtin "and" (logicalBinaryOp (&&) . checkBinary)),
      ("or", Builtin "or" (logicalBinaryOp (||) . checkBinary)),
      ("not", Builtin "not" (logicalSingularOp not . checkSingular)),
      ("error", Builtin "error" (mispError . checkSingular)),
      ("++", Builtin "++" (pure . String . stringConcat))
    ]

stringConcat :: [Expr] -> String
stringConcat ab =
  case ab of
    [] -> ""
    String x : xs -> x ++ stringConcat xs
    _ -> error "Expect two strings."

mispError :: Expr -> IO Expr
mispError xs = error $ "error: " ++ mispPrint xs

checkSingular :: [Expr] -> Expr
checkSingular es =
  if length es == 1
    then head es
    else error "Expect one argument."

logicalSingularOp :: (Bool -> Bool) -> Expr -> IO Expr
logicalSingularOp op x =
  case x of
    Boolean x -> pure $ Boolean (op x)
    _ -> error "Expect one boolean."

logicalBinaryOp :: (Bool -> Bool -> Bool) -> (Expr, Expr) -> IO Expr
logicalBinaryOp op (a, b) =
  case (a, b) of
    (Boolean a, Boolean b) -> pure $ Boolean (op a b)
    _ -> error "Expect two booleans."

comparisonBinaryOp :: (Double -> Double -> Bool) -> (Expr, Expr) -> IO Expr
comparisonBinaryOp op (a, b) =
  case (a, b) of
    (Number a, Number b) -> pure $ Boolean $ op a b
    _ -> error "Expect two numbers."

mispPrint :: Expr -> String
mispPrint expr =
  case expr of
    Number n -> show n
    String s -> s
    Symbol s -> s
    Boolean True -> "true"
    Boolean False -> "false"
    List xs -> "(" ++ L.intercalate ", " (L.map mispPrint xs) ++ ")"
    Builtin name _ -> "<builtin fn " ++ name ++ ">"
    Function name _ _ -> "<fn " ++ name ++ ">"
    _ -> error "Unprintable."

mispPrintMulti :: [Expr] -> IO ()
mispPrintMulti xs =
  case xs of
    [] -> putStr "\n"
    [x] -> putStrLn (mispPrint x)
    x : xs' -> putStr (mispPrint x ++ " ") >> mispPrintMulti xs'

checkBinary :: [Expr] -> (Expr, Expr)
checkBinary exprs =
  if length exprs /= 2
    then
      error "Wrong number of arguments supplied."
    else
      (head exprs, last exprs)

checkSndNotZero :: (Expr, Expr) -> (Expr, Expr)
checkSndNotZero (a, Number b) =
  if b == 0
    then
      error "Divide by zero."
    else
      (a, Number b)
checkSndNotZero x = x

numericBinaryOp :: (Double -> Double -> Double) -> (Expr, Expr) -> IO Expr
numericBinaryOp op exps =
  case exps of
    (Number a, Number b) -> pure $ Number (op a b)
    _ -> error "Expect two Numbers."
