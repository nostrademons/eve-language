module TypeCheck(typeCheck) where
import Control.Monad (ap)
import Control.Monad.Error
import Control.Monad.State
import Data
import Primitives

type Constraints = [(EveType, EveType)]
type Subst = TEnv

type ConstraintM a = StateT (Int, Constraints) EveM a
runConstraintM = flip runStateT (0, [])
addConstraint expected inferred = addConstraints [(expected, inferred)]
addConstraints new = modify (\(n, old) -> (n, new ++ old))
newTVar = do
    (n, constraints) <- get
    put (n + 1, constraints)
    return . TVar $ "__" ++ show n

instantiateType :: EveType -> ConstraintM EveType
instantiateType = substVars newTVar

sameType :: [EveType] -> ConstraintM EveType
sameType args = addConstraints (ap zip tail args) >> return (args !! 0)

buildConstraints :: TEnv -> EveExpr -> ConstraintM EveType
buildConstraints env (Literal (Int _)) = return tInt
buildConstraints env (Literal (Bool _)) = return tBool
buildConstraints env (Literal (String _)) = return tString
buildConstraints env (ListLiteral args) =
  if length args == 0 
    then newTVar >>= return . tList
    else mapM (buildConstraints env) args >>= sameType >>= return . tList
buildConstraints env (Variable var) = 
    maybe (throwError $ UnboundVar var) return $ lookup var env
buildConstraints env (Cond args) = mapM clauseType args >>= sameType
  where
    clauseType (pred, body) = do
      predType <- buildConstraints env pred
      bodyType <- buildConstraints env body
      addConstraint predType tBool
      return bodyType
buildConstraints env (Funcall fn args) = do
    fnType <- buildConstraints env fn
    argTypes <- mapM (buildConstraints env) args
    returnType <- newTVar
    addConstraint (tFunc argTypes returnType) fnType
    return returnType
buildConstraints env (Lambda args body) = do
    argTypes <- sequence $ replicate (length args) newTVar
    bodyType <- buildConstraints (zip args argTypes ++ env) body
    return $ tFunc argTypes bodyType

-- First arg should be a TVar, though nothing bad happens otherwise; it just does nothing
substVar from to haystack@(TPrim _) = haystack
substVar from to var@(TVar _) = if var == from then to else var
substVar from to (TCon name args) = TCon name $ map (substVar from to) args

substAll :: EveType -> EveType -> Constraints -> Constraints
substAll from to = map substPair 
  where substPair (x, y) = (substVar from to x, substVar from to y)

applySubst :: EveType -> Subst -> EveType
applySubst = foldl doSubst 
  where doSubst inType (from, to) = substVar (TVar from) to inType

occursIn :: EveType -> EveType -> Bool
occursIn needle (TPrim _) = False
occursIn needle haystack@(TVar _) = needle == haystack
occursIn needle (TCon _ params) = any (occursIn needle) params

unify :: (MonadError EveError m) => Constraints -> m Subst
unify [] = return []
unify ((s@(TPrim _), t@(TPrim _)):rest) 
    | s == t = unify rest
    | otherwise = throwError $ TypeMismatch s t
unify ((s@(TVar sVar), t):rest)
    | s `occursIn` t = throwError TypeCircularity
    | otherwise = unify (substAll s t rest) >>= return . ((sVar, t) :)
unify ((s, t@(TVar tVar)):rest)
    | s `occursIn` t = throwError TypeCircularity
    | otherwise = unify (substAll t s rest) >>= return . ((tVar, s) :)
unify ((s@(TCon name1 args1), t@(TCon name2 args2)):rest)
    | name1 /= name2 = throwError $ TypeMismatch s t
    | length args1 /= length args2 = throwError $ TypeMismatch s t
    | otherwise = unify $ zip args1 args2 ++ rest
unify ((s, t):rest) = throwError $ TypeMismatch s t

normalizeNames :: EveType -> EveType
normalizeNames typeExpr = evalState (substVars takeTVar typeExpr) ['A'..]
  where
    takeTVar = do
      nextVar:rest <- get
      put rest
      return $ TVar [nextVar]

substVars :: Monad m => m EveType -> EveType -> m EveType
substVars takeTVar typeExpr = evalStateT (normalize typeExpr) []
  where
    addTVar from newVar = modify ((from, newVar) :) >> return newVar
    swapTVar (TVar from) Nothing = lift takeTVar >>= addTVar from
    swapTVar (TVar from) (Just to) = return to
    normalize (TPrim name) = return $ TPrim name
    normalize (TVar var) = get >>= return . lookup var >>= swapTVar (TVar var)
    normalize (TCon name params) = mapM normalize params >>= return . TCon name

typeCheck :: TEnv -> EveExpr -> EveM EveType
typeCheck env expr = do
    (exprType, (_, constraints)) <- runConstraintM $ buildConstraints env expr
    subst <- unify constraints
    return . normalizeNames $ applySubst exprType subst
