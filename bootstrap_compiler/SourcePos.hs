module SourcePos(Pos(..), HasPos) where

data SourcePos = Pos {
    file :: String,
    offset :: !Int,
    line :: !Int,
    col :: !Int
} deriving (Eq)

defaultPos = Pos "" 0 0 0

instance Show SourcePos where
  show (Pos file offset line col) = file ++ ":" ++ show line ++ ":" ++ show col

class HasPos a where
    pos :: a -> SourcePos
