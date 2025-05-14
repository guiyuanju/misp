module Evaluator where

import qualified Data.List as L
import qualified Data.Map as M
import Model

apply :: (Env, [String], Program) -> [Expr] -> IO Expr
apply (envi, params, prog) args =
  if length params /= length args
    then error "Function argmuments number mismatch."
    else evalProg envi' prog
  where
    envi' = L.foldl' (\e (s, expr) -> insert e s expr) (pushEnv M.empty envi) (zip params args)

pushEnv :: M.Map String Expr -> Env -> Env
pushEnv m (Env env) = Env (m : env)

resolve :: Env -> String -> Expr
resolve (Env m) name =
  case search (Env m) name of
    Nothing -> error ("Unknwon symbol " ++ name ++ ".")
    Just e -> e

search :: Env -> String -> Maybe Expr
search (Env m) name =
  case m of
    [] -> Nothing
    envi : rest ->
      case M.lookup name envi of
        Nothing -> search (Env rest) name
        Just e -> Just e

define :: Env -> String -> Expr -> Env
define (Env m) name definition =
  case m of
    [] -> insert (Env m) name definition
    envi : _ -> case M.lookup name envi of
      Nothing -> insert (Env m) name definition
      Just _ -> error ("Varialbe '" ++ name ++ "' is already defined.")

insert :: Env -> String -> Expr -> Env
insert (Env m) name definition =
  case m of
    [] -> Env [M.insert name definition M.empty]
    envi : rest -> Env (M.insert name definition envi : rest)

setVar :: Env -> String -> Expr -> Env
setVar (Env m) name definition =
  case search (Env m) name of
    Nothing -> error ("Variable '" ++ name ++ "' not defined.")
    Just _ -> insert (Env m) name definition

evalExpr :: Env -> Expr -> IO (Env, Expr)
evalExpr envi expr =
  case expr of
    Number _ -> pure (envi, expr)
    Symbol s -> pure (envi, resolve envi s)
    String _ -> pure (envi, expr)
    Boolean b -> pure (envi, expr)
    List es -> do
      case es of
        [] -> pure (envi, List [])
        op : rest -> do
          (_, evaledOp) <- evalExpr envi op
          case evaledOp of
            Builtin _ f -> do
              args <- mapM (evalExpr envi) rest
              res <- f (map snd args)
              return (envi, res)
            Function _ envi' (params, body) -> do
              args <- mapM (evalExpr mergedEnv) rest
              res <- apply (mergedEnv, params, body) (map snd args)
              return (envi, res)
              where
                mergedEnv = mergeEnv envi' envi
            _ -> error ("First Element " ++ show evaledOp ++ " is not callable.")
    Builtin _ _ -> pure (envi, expr)
    Function name _ inner -> pure (envi, Function name envi inner)
    Def name body -> do
      (_, definition) <- evalExpr envi body
      return (define envi name definition, definition)
    Set name body -> do
      (_, definition) <- evalExpr envi body
      return (setVar envi name definition, definition)
    If cond trueBranch falseBranch -> do
      (_, condVal) <- evalExpr envi cond
      (_, res) <- if isTruethy condVal then evalExpr envi trueBranch else evalExpr envi falseBranch
      return (envi, res)
    Prog es -> do
      res <- evalProg envi (Program es)
      return (envi, res)
    While cond body -> do
      (_, cond') <- evalExpr envi cond
      if isTruethy cond'
        then do
          _ <- evalProg envi (Program body)
          evalExpr envi (While cond body)
        else pure (envi, List [])

mergeEnv :: Env -> Env -> Env
mergeEnv (Env front) (Env end) = Env (front ++ end)

isTruethy :: Expr -> Bool
isTruethy e =
  case e of
    Boolean True -> True
    _ -> False

evalProg :: Env -> Program -> IO Expr
evalProg envi (Program prog) = go envi' prog
  where
    envi' = pushEnv M.empty envi
    go _ [] = pure $ List []
    go e [expr] = do
      (envi', expr') <- evalExpr e expr
      seq envi' (return expr')
    go e (expr : exprs) = do
      (envi', _) <- evalExpr e expr
      evalProg envi' (Program exprs)
