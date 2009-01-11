module Types(Tyvar(Tyvar), Type(..), UnificationError(..), 
             Assumptions, defaultAssumptions, tBool, tInt, tString) where
import Control.Monad.Error hiding (join)

import Utils
import SourcePos
import Literal

data Tyvar = Tyvar String deriving (Eq)

instance Show Tyvar where 
    show (Tyvar name) = name

data Type =
    TNamed String
  | TVar Tyvar
  | TTuple [Type]
  | TFunc [Type] Type

instance Show Type where
    show (TNamed name) = name
    show (TVar tyvar) = show tyvar
    show (TTuple fields) = showTuple fields
    show (TFunc args ret) = "(" ++ join ", " (map show args) ++ " -> " ++ show ret ++ ")"

instance Eq Type where
    TNamed name1 == TNamed name2 = name1 == name2
    TVar tyvar1 == TVar tyvar2 = tyvar1 == tyvar2
    TTuple x1 == TTuple x2 = eqTuple x1 x2
    TFunc args1 ret1 == TFunc args2 ret2 = ret1 == ret2 && eqTuple args1 args2
    _ == _ = False

tBool = TNamed "Bool"
tInt = TNamed "Int"
tString = TNamed "String"

type Assumptions = [(String, Type)]

intBinop = TFunc [tInt, tInt] tInt
intBoolBinop = TFunc [tInt, tInt] tBool
boolBinop = TFunc [tBool, tBool] tBool

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
