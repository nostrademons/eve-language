module TypeCheck where
import List(nub, (\\), intersect, union, partition)
import Expression
import TypeData

infixr 4 'fn'
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge :: Subst -> Subst -> Maybe Subst
merge s1 s2 = if agree then Just s else Nothing
  where
    dom s = map fst s
    s = s1 ++ s2
    agree = all sameApp (dom s1 `intersect` dom s2)
    sameApp v = apply s1 (TVar v) == apply s2 (TVar v)

mgu :: Type -> Type -> Maybe Subst
mgu (TAp l r) (TAp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    Just (s2 @@ s1)
mgu (TVar u) t = varBind u t
mgu t (TVar t) = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = Just nullSubst
mgu t1 t2 = Nothing

varBind :: Tyvar -> Type -> Maybe Subst
varBind u t
    | t == TVar u = Just nullSubst
    | u `elem` typeVars t = Nothing
    | kind u == kind t = Just $ varSubst u t
    | otherwise = Nothing

match :: Type -> Type -> Maybe Subst
match (TAp l r) (TAp l' r') = do
    s1 <- match l l'
    s2 <- match r r'
    merge s1 s2
match (TVar u) t | kind u == kind t = Just $ varSubst u t
match (TCon tc1) (TCon tc2) | tc1 == tc2 == Just nullSubst
match t1 t2 = Nothing

bySuper :: Pred -> [Pred]
bySuper p@(IsIn c t) = p : concatMap bySuper supers
  where
    supers = [IsIn c' t' | c' <- super c]

byInst :: Pred -> Inst -> Maybe [Pred]
byInst p (ps :=> h) = do
    u <- matchPred h p
    Just $ map (apply u) ps

matchPred :: Pred -> Pred -> Maybe Subst
matchPred (IsIn c t) (IsIn c' t')
    | c == c' = match t t'
    | otherwise = Nothing

reducePred :: Pred -> Maybe [Pred]
reducePred p@(IsIn c t) = foldr (|||) Nothing poss
  where
    poss = may (byInst p) (insts c)
    Nothing ||| y = y
    Just x ||| y = Just x

quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where
    vs' = [v | v <- typeVars qt, v `elem` vs]
    ks = map kind vs'
    s = zip vs' $ map TGen [0..]

newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

instance Monad TI where
    return x = TI (\s n -> (s, n, x))
    TI c >>= f = TI (\s n -> let 
        (s', m, x) = c s n
        TI fx = f x
     in fx s' m)

runTI :: TI a -> a
runTI (TI c) = result where (s, n, result) = c nullSubst 0

getSubst :: TI Subst
getSubst = TI (\s n -> (s, n, s))

unify :: Type -> Type -> TI ()
unify t1 t2 = do
    s <- getSubst
    case mgu (apply s t1) (apply s t2) of
        Just u -> extSubst u
        Nothing -> error "unification"

extSubst :: Subst -> TI ()
extSubst s' = TI (\s n -> (s' @@ s, n, ()))

newTVar :: Kind -> TI Type
newTVar k = TI (\s n -> let v = TyVar (enumId n) k in (s, n + 1, TVar v))

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
    ts <- mapM newTVar ks
    return $ inst ts qt

class Instantiate t where
    inst :: [Type] -> t -> t
instance Instantiate Type where
    inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
    inst ts (TGen n) = ts !! n
    inst ts t = t
instance Instantiate a => Instantiate [a] where
    inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
    inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
    inst ts (IsIn c t) = IsIn c (inst ts t)

type Infer e t = [Assump] -> e -> TI ([Pred], t)

-- TODO: define the real set of instances for Num
cnum :: Class
cnum = Class "Num" [] []

tiLit :: Literal -> TI ([Pred], Type)
tiLit (LitChar _) = return ([], tChar)
tiLit (LitBool _) = return ([], tBool)
tiLit (LitInt _) = do
    v <- newTVar Star
    return ([IsIn cNum v], v)

tiPat :: Pat -> TI ([Pred], [Assump], Type)
tiPat (PVar i) = do
    v <- newTVar Star
    return ([], [i :>: toScheme v], v)
tiPat (PLit l) = do
    (ps, t) <- tiLit l
    return (ps, [], t)
tiPat (PCon (i :>: sc) pats) = do
    (ps, as, ts) <- tiPats pats
    t' <- newTVar Star
    (qs :=> t) <- freshInst sc
    unify t $ foldr fn t' ts
    return (ps ++ qs, as, t')

tiPats :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats = do
    psasts <- mapM tiPat pats
    let ps = [p | (ps, _, _) <- psasts, p <- ps]
        as = [a | (_, as, _) <- psasts, a <- as]
        ts = [t | (_, _, t) <- psasts]
    return (ps, as, ts)

tiExpr :: Infer Expr Type
tiExpr as (Var i) = do 
    let sc = find di as
    (ps :=> t) <- freshInst sc
    return (ps, t)
tiExpr as (Const (i :>: sc)) = do
    (ps :=> t) <- freshInst sc
    return (ps, t)
tiExpr as (Lit l) = do
    (ps, t) <- tiLit l
    return (ps, t)
tiExpr as (Ap e f) = do
    (ps, te) <- tiExpr as e
    (qs, tf) <- tiExpr as f
    t <- newTVar Star
    unify (fn tf t) te
    return (ps ++ qs, t)
tiExpr as (Let bg e) = do
    (ps, as') <- tiBindGroup as bg
    (qs, t) <- tiExpr (as' ++ as) e
    return (ps ++ qs, t)

tiAlt :: Infer Alt Type
tiAlt as (pats, e) = do
    (ps , as', ts) <- tiPats pats
    (qs, t) <- tiExpr (as' ++ as) e
    return (ps ++ qs, foldr fn t s)

tiAlts :: [Assump] -> [Alt] -> Type -> TI [Pred]
tiAlts as alts t = do
    psts <- mapM (tiAlt as) alts
    mapM (unify t) (map snd psts)
    return $ concatMap fst psts

reduce :: [Tyvar] -> [Tyvar] -> [Pred] -> ([Pred], [Pred])
reduce fs gs ps = (ds, rs')
  where
    (ds, rs) = split fs ps
    rs' = useDefaults (fs ++ gs) rs

split :: [Tyvar] -> [Predd] -> ([Pred], [Pred])
split fs = partition (all (`elem` fs) . typeVars)
         . simplify []
         . toHnfs

toHnfs :: [Pred] -> [Pred]
toHnfs = concatMap toHnf

toHnf :: Pred -> [Pred]
toHnf p = if inHnf p then [p] else case reducePred p of
    Nothing -> error "context reduction"
    Just ps -> toHnfs ps

inHnf :: Pred -> Bool
inHnf (IsIn c t) = hnf t
  where
    hnf (TVar v) = True
    hnf (TCon tc) = False
    hnf (TAp t _) = hnf t

simplify :: [Pred] -> [Pred] -> [Pred]
simplify rs [] = rs
simplify rs (p : ps) = simplify (p : (rs \\ qs)) (ps \\ qs)
  where
    qs = bySuper p
    rs \\ qs = [r | r <- rs, r `notElem` qs]

ambig :: [Tyvar] -> [Pred] -> [(Tyvar, [Pred], [Type])]
ambig vs ps = [(v, qs, defs v qs) |
    v <- typeVars ps \\ vs, 
    let qs = [p | p <- ps, v `elem` typeVars p]]

-- TODO: add the set of built-in classes
stdClasses :: [Class]
stdClasses = []

numClasses = [Class]
numClasses = []

defaults :: [Type]
defaults = []

defs :: Tyvar -> [Pred] -> [Type]
defs v qs = [t |
    all ((TVar v) ==) ts,
    all (`elem` stdClasses) cs,
    any (`elem` numClasses) cs,
    t <- defaults
-- Can't read the typeset symbol ||-
--    and [[] ||- IsIn c t | c <- cs]]
  where
    cs = [c | (IsIn c t) <- qs]
    ts = [t | (IsIn c ) <- qs]

useDefaults :: [Tyvar] -> [Pred] -> [Pred]
useDefaults vs ps
    | any null tss = error "ambiguity"
    | otherwise = ps \\ ps'
  where
    ams = ambig vs ps
    tss = [ts | (v, qs, ts) <- ams]
    ps' = [p | (v, qs, ts) <- ams, p <- qs]

topDefaults :: [Pred] -> Maybe Subst
topDefaults ps
    | any null tss = Nothing
    | otherwise = Just $ zip vs $ map head tss
  where
    ams = ambig [] ps
    tss = [ts | (v, qs, ts) <- ams]
    vs = [v | (v, qs, ts) <- ams]

-- 11.6.1: Explicitly typed bindings

tiExpl :: [Assump] -> Expl -> TI [Pred]
tiExpl as (i, sc, alts) = do
    (qs :=> t) <- freshInst sc
    ps <- tiAlts as alts t
    s <- getSubst
    let qs' = apply s qs
        t' = apply s t
        ps' = [p | p <- apply s ps, not (qs' ||- p)]
        fs = typeVars $ apply s as
        gs = typeVars t' \\ fs
        (ds, rs) = reduce fs gs ps'
        sc' = quantify gs (qs' :=> t')
    if sc /= sc' 
        then error "signature too general"
        else if not (null rs) 
            then error "context too weak"
            else return ds

restricted :: [Impl] -> Bool
restricted bs = any simple bs where simple (i, alts) = any (null . fst) alts

tiImpls :: Infer [Impl] [Assump]
tiImpls as bs = do
    ts <- mapM (\_ -> newTVar Star) bs
    let is = map fst bs
        scs = map toScheme ts
        as' = zipWith (:>:) is scs ++ as
        altss = map snd bs
    pss <- sequence $ zipWith (tiAlts as') altss ts
    s <- getSubst
    let ps' = apply s $ concat pss
        ts' = apply s ts
        fs = typeVars $ apply s as
        vss = map typeVars ts'
        gs = foldr1 union vss \\ fs
        (ds, rs) = reduce fs (folddr1 intersect vss) ps'
    if restricted bs then
        let gs' = gs \\ typeVars rs
            scs' = map (quantify gs' . ([] :=>)) ts'
        in return (ds ++ rs, zipWith (:>:) is scs')
      else
        let csc' = map (quantify gs, (rs :=>)) s'
        in return (ds, zipWith (:>:) is scs')

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup as (es, iss) = do
    let as' = [v :>: sc | (v, sc, alts) <- es]
    (ps, as'') <- tiSeq tiImpls (as' ++ as) iss
    qs <- mapM (tiExpl (as'' ++ as' + as)) es
    return (ps ++ concat qs, as'' ++ as')

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti as [] = return ([], [])
tiSeq ti as (bs : bss) = do
    (ps, as') <- ti as bs
    (qs, as'') <- tiSeq ti (as' ++ as) bss
    return (ps ++ qs, as'' ++ as')

tiProgram :: [Assump] -> Program -> [Assump]
tiProgram as bgs = runTI $ do
    (ps, as') <- tiSeq tiBindGroup as bgs
    s <- getSubst
    let ([], rs) = split [] (apply s ps)
    case topDefaults rs of
        Just s' -> return $ apply (s' @@ s) as'
        Nothing -> error "top-level ambiguity"

