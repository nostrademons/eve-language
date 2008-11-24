module Types(Type(..)) where
import Utils
import Literal

data Type =
    TNamed String
  | TTuple [Type]
  | TFunc [Type] Type

instance Show Type where
  show (TNamed name) = name
  show (TTuple fields) = showTuple fields
  show (TFunc args ret) = "(" ++ join ", " (map show args) ++ " -> " ++ show ret ++ ")"

instance Eq Type where
  TNamed name1 == TNamed name2 = name1 == name2
  TTuple x1 == TTuple x2 = eqTuple x1 x2
  TFunc args1 ret1 == TFunc args2 ret2 = ret1 == ret2 && eqTuple args1 args2

