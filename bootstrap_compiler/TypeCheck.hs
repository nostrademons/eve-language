module TypeCheck(typeCheck) where

import Control.Monad (ap)
import Control.Monad.State
import Control.Monad.Error
import List
import Maybe

import Types
import Expr
import Error
import Literal

class Types t where
    apply :: Subst -> t -> t
    typeVars :: t -> [Tyvar]

instance Types Type where
    apply s (TVar var) = maybe (TVar var) id $ lookup var s
    apply s (TCon tc) = TCon tc
    apply s (TAp t args) = TAp (apply s t) $ map (apply s) args

    typeVars (TVar var) = [var]
    typeVars (TCon tc) = []
    typeVars (TAp t args) = nub $ concatMap typeVars (t : args)

instance Types a => Types [a] where
    apply s = map (apply s)
    typeVars = nub . concatMap typeVars

type Subst = [(Tyvar, Type)]

nullSubst :: Subst
nullSubst = []

varSubst :: Tyvar -> Type -> Subst
varSubst from to = [(from, to)]

composeSubsts :: Subst -> Subst -> Subst
composeSubsts s1 s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

mergeSubsts :: Subst -> Subst -> Maybe Subst
mergeSubsts s1 s2 = if agree then Just s else Nothing
  where
    dom s = map fst s
    s = s1 `union` s2
    agree = all (\var -> apply s1 (TVar var) == apply s2 (TVar var)) (dom s1 `intersect` dom s2)

unifyTypes :: [Type] -> [Type] -> Either UnificationError Subst
unifyTypes t1 t2 = liftM (foldr composeSubsts nullSubst) $ sequence $ zipWith mgu t1 t2

mgu :: Type -> Type -> Either UnificationError Subst
-- Todo: unify the parameters
mgu (TVar var) t = varBind var t
mgu t (TVar var) = varBind var t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
mgu (TAp t1 args1) (TAp t2 args2) = unifyTypes (t1 : args1) (t2 : args2)
mgu t1 t2 = Left $ UnificationMismatch t1 t2

varBind :: Tyvar -> Type -> Either UnificationError Subst
varBind var t
  | t == TVar var = return nullSubst
  | var `elem` typeVars t = Left $ UnificationCircularity t
  | otherwise = return $ varSubst var t

data TypeMState = TypeMState {
    typeSubst :: Subst,
    typeN :: Int
}

type TypeM = StateT TypeMState (Either EveError)

extendSubst :: Subst -> TypeM ()
extendSubst s = modify $ \state -> state { typeSubst = composeSubsts s $ typeSubst state }

unify :: Expr -> Type -> Type -> TypeM ()
unify expr t1 t2 = do
    s <- liftM typeSubst get
    case mgu (apply s t1) (apply s t2) of
        Right s' -> extendSubst s'
        Left err -> throwError $ TypeError expr err

newTVar :: TypeM Type
newTVar = do
    n <- liftM typeN get
    modify $ \state -> state { typeN = n + 1 }
    return $ TVar $ Tyvar ("t" ++ show n) 0 -- TODO: kinds

typeCheckLiteral :: Literal -> TypeM Type
typeCheckLiteral (LitBool _) = return tBool
typeCheckLiteral (LitInt _) = return tInt
typeCheckLiteral (LitString _) = return tString

typeCheckExpr :: Assumptions -> Expr -> TypeM Type
typeCheckExpr tEnv expr@(Expr val pos Nothing) = typeCheckExprValue tEnv expr val
typeCheckExpr tEnv expr@(Expr val pos (Just (Scheme _ expected))) = do
    rawType <- typeCheckExprValue tEnv expr val
    state <- get
    let found = apply (typeSubst state) rawType
    if expected == found 
        then return expected 
        else throwError $ TypeError expr $ UnificationMismatch expected found

typeCheckExprValue :: Assumptions -> Expr -> ExprValue -> TypeM Type
typeCheckExprValue tEnv _ (Literal lit) = typeCheckLiteral lit
typeCheckExprValue tEnv _ (TupleLiteral exprs) = liftM tTuple $ mapM (typeCheckExpr tEnv) exprs
-- TODO: Need record predicates to typecheck records
typeCheckExprValue tEnv expr (Variable var) = 
    maybe (throwError $ TypeError expr $ UnboundVar var) return $ lookup var tEnv
typeCheckExprValue tEnv expr (Funcall fn args) = do
    tFn <- typeCheckExpr tEnv fn
    tArgs <- mapM (typeCheckExpr tEnv) args
    tResult <- newTVar
    unify expr (tFunc tArgs tResult) tFn
    return tResult

typeCheck :: Expr -> Either EveError Type
typeCheck expr = do
    let typeM = typeCheckExpr defaultAssumptions expr 
    let initialState = TypeMState nullSubst 0
    (exprType, state) <- runStateT typeM initialState
    return $ apply (typeSubst state) exprType
