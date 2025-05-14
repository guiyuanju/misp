module Main where

import Builtins
import Control.Exception (ErrorCall, catch)
import qualified Data.Map as M
import Evaluator
import Model
import Parser

env :: Env
env = Env [builtins]

program :: Program
program =
  Program
    [ Number 1,
      String "hi",
      Symbol "+",
      -- List [Symbol "+", String "asd", Number 3],
      Def "ten" (Number 10),
      Symbol "ten",
      List [Symbol "println", Number 2, Number 3],
      List [Symbol "println", Symbol "ten"],
      -- List [Symbol "println", Symbol "id", Number 3],
      Def "add" (Function "add" (Env []) (["a", "b"], Program [List [Symbol "+", Symbol "a", Symbol "b"]])),
      List [Symbol "println", Symbol "add"],
      List [Symbol "println", List [Symbol "add", Number 1, Number 100]]
      -- List [Symbol "/", Number 1, Number 0]
    ]

errorHandler :: ErrorCall -> IO Expr
errorHandler e = print e >> pure (List [])

eval :: Program -> IO Expr
eval program = catch (evalProg env program) errorHandler

isDbg = False

dbg :: (Show a) => a -> IO a
dbg x =
  if isDbg
    then print x >> return x
    else return x

main :: IO ()
main = do
  content <- readFile "main.ms"
  res <- case readExpr content of
    Left err -> return $ show err
    Right prog -> show <$> eval prog
  print res
  return ()
