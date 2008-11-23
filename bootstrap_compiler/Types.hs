module Types(Type(..)) where
import Utils
import Literal

data Type =
    TPrim String
  | TLiteral Literal
  | TTuple [Type]
  | TOr [Type]
  | TRecord [(String, Type)]
  | TFunc [Type] Type

instance Show Type where
  show (TPrim name) = name
  show (TLiteral datum) = show datum
  show (TTuple fields) = showTuple fields
  show (TRecord fields) = showRecord fields
  show (TFunc args ret) = "(" ++ join ", " (map show args) ++ " -> " ++ show ret ++ ")"

instance Eq Type where
  TPrim name1 == TPrim name2 = name1 == name2
  TLiteral x1 == TLiteral x2 = x1 == x2
  TTuple x1 == TTuple x2 = eqTuple x1 x2
  TRecord x1 == TRecord x2 = eqRecord x1 x2
  TFunc args1 ret1 == TFunc args2 ret2 = ret1 == ret2 && eqTuple args1 args2

