module Literal(Literal(..)) where

data Literal = 
    LitInt { litInt :: Int }
  | LitBool { litBool :: Bool }
  | LitString { litString :: String }
  deriving (Eq)

instance Show Literal where
    show (LitInt i) = show i
    show (LitBool b) = show b
    show (LitString s) = show s
