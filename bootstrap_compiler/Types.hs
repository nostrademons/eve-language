module Types(Tyvar(Tyvar), Tycon(Tycon), Type(..), 
             Constraint(..), Scheme(Scheme), UnificationError(..), 
             Assumptions, defaultAssumptions, 
             tBool, tInt, tString, tTuple, tFunc) where
import Control.Monad.Error hiding (join)

import Utils
import SourcePos
import Literal

type Kind = Int
data Tyvar = Tyvar String Kind deriving (Eq)
data Tycon = Tycon String Kind deriving (Eq)

instance Show Tyvar where show (Tyvar name _)= name
instance Show Tycon where show (Tycon name _) = name

data Type =
    TVar Tyvar
  | TCon Tycon
  | TAp Type [Type]

instance Show Type where
    show (TVar tyvar) = show tyvar
    show (TCon tycon) = show tycon
    show (TAp (TCon (Tycon "Func" _)) args) = 
        "(" ++ showCommas (tail args) ++ " -> " ++ show (head args) ++ ")"
    show (TAp (TCon (Tycon "Tuple" _)) args) = showTuple args
    show (TAp t args) = show t ++ "<" ++ showCommas args ++ ">"

instance Eq Type where
    TVar t1 == TVar t2 = t1 == t2
    TCon t1 == TCon t2 = t1 == t2
    TAp t1 args1 == TAp t2 args2 = t1 == t2 && eqTuple args1 args2
    _ == _ = False

nullKind = 0
tBool = TCon $ Tycon "Bool" nullKind
tInt = TCon $ Tycon "Int" nullKind
tString = TCon $ Tycon "String" nullKind
tTuple components = TAp (TCon $ Tycon "Tuple" $ length components) components
tFunc args ret = TAp (TCon $ Tycon "Func" $ length params) params
  where params = ret : args

type Class = String

data Constraint = 
    IsIn Class Type 
  | HasField (String, Type) Type
    deriving(Eq)

instance Show Constraint where
    show (IsIn cls t) = cls ++ "(" ++ show t ++ ")"
    show (HasField field _) = showPair field

data Scheme = 
    Scheme [Constraint] Type
    deriving(Eq)

instance Show Scheme where
    show (Scheme constraints t) = show t ++ 
        (if null constraintStrings then "" else " where " ++ showCommas constraintStrings)
      where
        constraintStrings :: [String]
        constraintStrings = [] -- TODO

type Assumptions = [(String, Type)]

intBinop = tFunc [tInt, tInt] tInt
intBoolBinop = tFunc [tInt, tInt] tBool
boolBinop = tFunc [tBool, tBool] tBool

defaultAssumptions = [
    ("pow", intBinop),
    ("mul", intBinop),
    ("div", intBinop),
    ("add", intBinop),
    ("sub", intBinop),
    ("eq", intBoolBinop),
    ("ne", intBoolBinop),
    ("lt", intBoolBinop),
    ("le", intBoolBinop),
    ("gt", intBoolBinop),
    ("ge", intBoolBinop),
    ("and_", boolBinop),
    ("or_", boolBinop)]

data UnificationError =
    UnificationCircularity Type
  | UnificationMismatch Type Type
  | UnboundVar String
  | StupidEitherMonadNeedsADefault String
   deriving (Eq)

instance Show UnificationError where
    show (UnificationCircularity t) = "Type circularity: " ++ show t
    show (UnificationMismatch expected found) = 
        concat ["Type mismatch: ", show expected, ", found ", show found]
    show (UnboundVar var) = concat ["Unbound variable ", var]

instance Error UnificationError where
    noMsg = StupidEitherMonadNeedsADefault ""
    strMsg = StupidEitherMonadNeedsADefault
