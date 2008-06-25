module Primitives(primitiveEnv) where
import Data
import Utils
import Control.Monad.Error

primitiveEnv = zip names $ map (uncurry Primitive) (zip names values)
    where (names, values) = unzip primitives

primitives :: [(String, [EveData] -> EveData)]
primitives = [
  ("\\**", numericBinop ((^) . fromIntegral)),
  ("\\*", numericBinop (*)),
  ("\\/", numericBinop div),
  ("\\%", numericBinop mod),
  ("\\+", numericBinop (+)),
  ("\\-", numericBinop (-)),
  ("\\<", numBoolBinop (<)),
  ("\\>", numBoolBinop (>)),
  ("\\==", numBoolBinop (==)),
  ("\\!=", numBoolBinop (/=)),
  ("\\>=", numBoolBinop (>=)),
  ("\\<=", numBoolBinop (<=)),
  ("\\and", boolBoolBinop (&&)),
  ("\\or", boolBoolBinop (||)),
  ("\\not", boolOp (not)),

  ("\\&", concatString),

  ("head", car),
  ("tail", cdr),
  ("cons", cons),
  ("get", getList),
  ("slice", sliceList)]

numericBinop :: (Int -> Int -> Int) -> [EveData] -> EveData
numericBinop f [Int arg1, Int arg2] = Int $ f arg1 arg2

numBoolBinop f [Int arg1, Int arg2] = Bool $ f arg1 arg2
boolBoolBinop f [Bool arg1, Bool arg2] = Bool $ f arg1 arg2
boolOp f [Bool arg] = Bool $ f arg

concatString [String xs, String ys] = String (xs ++ ys)

car [List (x:xs)] = x
cdr [List (x:xs)] = List xs
cons [x, List xs] = List (x:xs)
getList [Int index, List xs] = xs !! realIndex
  where
    realIndex = if index < 0 then length xs + index else index
sliceList [Int start, Int end, List xs] = List (strip first last xs)
  where 
    first = if start < 0 then length xs + start else start
    last = if end < 0 then -end else length xs - end
