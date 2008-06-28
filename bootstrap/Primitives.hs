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

  ("head", car),
  ("tail", cdr),
  ("cons", cons),
  ("get", getList),
  ("slice", sliceList)]

primitiveMultiMethods = [
  ("\\&", [concatList, concatString]),
  ("len", [len]),

  ("Int", [makeInt]),
  ("Bool", [makeBool]),
  ("Str", [makeString]),
  ("Sym", [makeSymbol]),
  ("List", [makeList List]),
  ("Tuple", [makeList Tuple]),
  ("Record", [typeObject]),
  ("Function", [typeObject])]

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

len [String xs] = return . Int $ length xs
len [List xs] = return . Int $ length xs
len [Tuple xs] = return . Int $ length xs
len [Record xs] = return . Int $ length xs
len _ = typeError "Length requires a sequence or container"

concatString [String xs, String ys] = return $ String (xs ++ ys)
concatString _ = typeError "Concatenation needs a sequence"

concatList [List xs, List ys] = return $ List (xs ++ ys)
concatList _ = typeError "Concatenation needs a sequence"

typeObject _ = typeError "Pure type objects cannot be applied."

makeInt [Int x] = return . Int $ x
makeInt [Bool x] = return . Int $ if x then 1 else 0
makeInt [String x] = return . Int $ read x
makeInt _ = typeError "Int expects a single Int, Bool, or String"

makeBool [Int x] = return . Bool $ x /= 0
makeBool [Bool x] = return . Bool $ x
makeBool [String x] = return . Bool $ length x > 0
makeBool [Tuple xs] = return . Bool $ length xs > 0
makeBool [List xs] = return . Bool $ length xs > 0
makeBool _ = typeError "Bool expects a single Int, Bool, String, Tuple, or List"

makeString [x] = return . String $ show x
makeString _ = typeError "String expects a single argument"

makeSymbol [String x] = return $ Symbol x
makeSymbol _ = typeError "Sym expects a string"

makeList constr [Tuple xs] = return $ constr xs
makeList constr [x] = return $ constr [x]
makeList constr xs = return $ constr xs

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
