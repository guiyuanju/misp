module Model
  ( Program (Program),
    Expr (..),
    Env (Env),
  )
where

import qualified Data.Map as M

newtype Program = Program [Expr]
  deriving (Show)

data Expr
  = Number Double
  | String String
  | Boolean Bool
  | Symbol String
  | List [Expr]
  | Builtin String ([Expr] -> IO Expr)
  | Function String Env ([String], Program)
  | Def String Expr
  | Set String Expr
  | If Expr Expr Expr
  | Prog [Expr]
  | While Expr [Expr]

instance Show Expr where
  show (Number n) = "Number " ++ show n
  show (Symbol s) = "Symbol " ++ show s
  show (String s) = "String " ++ show s
  show (Boolean b) = "Bool " ++ show b
  show (List xs) = show xs
  show (Builtin name _) = "<builtin fn " ++ name ++ ">"
  -- show (Function name env body) = "<fn " ++ name ++ "{ " ++ show env ++ " }" ++ show body ++ ">"
  show (Function name env body) = "<fn " ++ name ++ ">"
  show (Def a b) = "def " ++ show a ++ " " ++ show b
  show (Set a b) = "set " ++ show a ++ " " ++ show b
  show (If c a b) = "if " ++ show c ++ " " ++ show a ++ " " ++ show b
  show (Prog es) = "prog " ++ show es
  show (While c b) = "while " ++ show c ++ " " ++ show b

newtype Env = Env [M.Map String Expr]

instance Show Env where
  show (Env m) = show m
