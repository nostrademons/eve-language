module Literal(Literal(..)) where

data Literal = 
    Int { litInt :: Int }
  | Bool { litBool :: Bool }
  | String { litString :: String }
  deriving (Eq)

instance Show Literal where
    show (Int i) = show i
    show (Bool b) = show b
    show (String s) = show s
