
type Id = String
enumId :: Int -> Id
enumId n = "v" ++ show n

data Kind = Star | Kfun Kind Kind deriving Eq

data Type = 
    TVar Tyvar
  | TCon Tycon
  | TAp Type Type
  | TGen Int
    deriving Eq

data Tyvar = Tyvar Id Kind deriving Eq
data Tycon = Tycon Id Kind deriving Eq

tChar = TCon (Tycon "Char" Star)
tInt = TCon (Tycon "Int" Star)
tBool = TCon (Tycon "Bool" Star)
tArrow = TCon (Tycon "(->)" (KFun Star (KFun Star Star))

class HasKind t where kind :: t -> Kind
instance HasKind TyVar where kind (Tyvar v k) = k
instance HasKind TyCon where kind (Tycon v k) = k
instance HasKind Type where
    kind (TCon tc) = kind tc
    kind (TVar u) = kind u
    kind (TAp t _) = case (kind t) of (Kfun _ k) -> k

type Subst = [(Tyvar, Type)]
nullSubst :: Subst
nullSubst = []

varSubst :: Subst
varSubst u t = [(u, t)]

class Types t where
    apply :: Subst -> t -> t
    typeVars :: t -> [Tyvar]

instance Types Type where
    apply s (TVar u) = case lookup u s of
        Just t -> t
        Nothing -> TVar u
    apply s (TAp l r) = TAp (apply s l) (apply s r)
    apply s t = t
    typeVars (TVar u) = [u]
    typeVars (TAp l r) = typeVars l `union` typeVars r
    typeVars t = []

instance Types a => Types [a] where
    apply s = map (apply s)
    typeVars = nub . concat . map typeVars

data Class = Class { name :: Id, super :: [Class], insts :: [Inst] }
type Inst = Qual Pred
instance Eq Class where c == c' = name c == name c'

data Qual t = [Pred] :=> t deriving Eq
data Pred = IsIn Class Type deriving Eq

instance Types t => Types (Qual t) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    typeVars (ps :=> t) = typeVars ps `union` typeVars t

instance Types Pred where
    apply s (InIn c ) = IsIn c (apply s t)
    typeVars (IsIn c t) = typeVars t

data Scheme = Forall [Kind] (Qual Type) deriving Eq

instance Types Scheme where
    apply s (Forall ks qt) = Forall ks (apply s qt)
    typeVars (Forall ks qt) = typeVars qt

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

data Assump = Id :>: Scheme

instance Types Assump where
    apply s (i :>: sc) = i :>: (apply s sc)
    typeVars (i :>: sc) = typeVars sc

find :: Id -> [Assump] -> Scheme
find i as = head [sc | (i' :>: sc) <- as, i == i']
