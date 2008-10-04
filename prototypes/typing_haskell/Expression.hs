module Expression where
import TypeData

data Literal = 
    LitInt Integer
  | LitChar Char
  | LitBool Bool

data Pattern =
    PVar Id
  | PLit Literal
  | PCon Assump [Pattern]

data Expr = 
    Var Id
  | Lit Literal
  | Const Assump
  | Ap Expr Expr
  | Let BindGroup Expr

type Alt = ([Pat], Expr)
type Expl = (Id, Scheme, [Alt])
type Impl = (Id, [Alt])
type BindGroup = ([Expl], [[Impl]])
type Program = [BindGroup]

