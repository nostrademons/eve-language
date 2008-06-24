module Primitives(primitiveEnv, primitiveTypes) where
import Data
import Utils
import Control.Monad.Error

tNumBinop = tFunc [tInt, tInt] tInt
tOrdBoolBinop = tFunc [tInt, tInt] tBool
tBoolBoolBinop = tFunc [tBool, tBool] tBool
tBoolOp = tFunc [tBool] tBool
tAList = tList (TVar "A")

primitiveEnv = zip names $ map (uncurry Primitive) (zip names values)
    where (names, values, _) = unzip3 primitives
primitiveTypes = zip names types where (names, _, types) = unzip3 primitives

primitives :: [(String, [EveData] -> EveData, EveType)]
primitives = [
  ("\\**", numericBinop ((^) . fromIntegral), tNumBinop),
  ("\\*", numericBinop (*), tNumBinop),
  ("\\/", numericBinop div, tNumBinop),
  ("\\%", numericBinop mod, tNumBinop),
  ("\\+", numericBinop (+), tNumBinop),
  ("\\-", numericBinop (-), tNumBinop),
  ("\\<", numBoolBinop (<), tOrdBoolBinop),
  ("\\>", numBoolBinop (>), tOrdBoolBinop),
  ("\\==", numBoolBinop (==), tOrdBoolBinop),
  ("\\!=", numBoolBinop (/=), tOrdBoolBinop),
  ("\\>=", numBoolBinop (>=), tOrdBoolBinop),
  ("\\<=", numBoolBinop (<=), tOrdBoolBinop),
  ("\\and", boolBoolBinop (&&), tBoolBoolBinop),
  ("\\or", boolBoolBinop (||), tBoolBoolBinop),
  ("\\not", boolOp (not), tBoolOp),


  ("head", car, tFunc [tAList] (TVar "A")),
  ("tail", cdr, tFunc [tAList] (tAList)),
  ("cons", cons, tFunc [TVar "A", tAList] (tAList)),
  ("slice", sliceList, tFunc [tInt, tInt, tAList] tAList)]

numericBinop :: (Int -> Int -> Int) -> [EveData] -> EveData
numericBinop f [Int arg1, Int arg2] = Int $ f arg1 arg2

numBoolBinop f [Int arg1, Int arg2] = Bool $ f arg1 arg2
boolBoolBinop f [Bool arg1, Bool arg2] = Bool $ f arg1 arg2
boolOp f [Bool arg] = Bool $ f arg

car [List (x:xs)] = x
cdr [List (x:xs)] = List xs
cons [x, List xs] = List (x:xs)
sliceList [Int start, Int end, List xs] = List (strip start last xs)
  where last = length xs - end
