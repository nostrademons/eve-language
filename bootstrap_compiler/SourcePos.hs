module SourcePos(SourcePos(Pos), defaultPos, HasPos, pos) where

data SourcePos = Pos {
    pos_file :: String,
    pos_offset :: !Int,
    pos_line :: !Int,
    pos_col :: !Int
} deriving (Eq)

defaultPos = Pos "" 0 0 0

instance Show SourcePos where
  show (Pos file offset line col) = file ++ ":" ++ show line ++ ":" ++ show col

class HasPos a where
    pos :: a -> SourcePos
