module Primitives(primitiveEnv) where
import Data
import Utils
import Control.Monad.Error

makePrimitives = zip names $ map (uncurry Primitive) (zip names values)
  where 
    (names, values) = unzip primitives

makeMultiMethods = map makeMultiMethod primitiveMultiMethods
  where
    makeMultiMethod (name, methods) = 
        (name, MultiMethod $ map (Primitive name) methods)

primitiveEnv = makePrimitives ++ makeMultiMethods


primitives :: [(String, [EveData] -> EveM EveData)]
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

  ("Sym", makeSymbol),
  ("List", makeList),
  ("head", car),
  ("tail", cdr),
  ("cons", cons),
  ("get", getList),
  ("slice", sliceList)]

primitiveMultiMethods = [
  ("\\&", [concatList, concatString])]

typeError = throwError . TypeError

numericBinop :: (Int -> Int -> Int) -> [EveData] -> EveM EveData
numericBinop f [Int arg1, Int arg2] = return $ Int $ f arg1 arg2
numericBinop f _ = typeError "Numeric operator expects 2 ints"

numBoolBinop f [Int arg1, Int arg2] = return $ Bool $ f arg1 arg2
numBoolBinop f _ = typeError "Boolean operator expects 2 ints"

boolBoolBinop f [Bool arg1, Bool arg2] = return $ Bool $ f arg1 arg2
boolBoolBinop f _ = typeError "Boolean operator expects 2 bools"

boolOp f [Bool arg] = return $ Bool $ f arg
boolOp f _ = typeError "Expects a boolean"

concatString [String xs, String ys] = return $ String (xs ++ ys)
concatString _ = typeError "Concatenation needs a sequence"

concatList [List xs, List ys] = return $ List (xs ++ ys)
concatList _ = typeError "Concatenation needs a sequence"

makeSymbol [String x] = return $ Symbol x
makeSymbol _ = typeError "Sym expects a string"

makeList [Tuple xs] = return $ List xs
makeList [x] = return $ List [x]
makeList xs = return $ List xs

car [List (x:xs)] = return $ x
car _ = typeError "head expects a list"

cdr [List (x:xs)] = return $ List xs
cdr _ = typeError "tail expects a list"

cons [x, List xs] = return $ List (x:xs)
cons _ = typeError "cons expects a value and a list"

getList [Int index, List xs] = return $ xs !! realIndex
  where
    realIndex = if index < 0 then length xs + index else index
getList _ = typeError "get expects an (int, List)"

sliceList [Int start, Int end, List xs] = return $ List (strip first last xs)
  where 
    first = if start < 0 then length xs + start else start
    last = if end < 0 then -end else length xs - end
sliceList _ = typeError "slice expects an (int, int, List)"
