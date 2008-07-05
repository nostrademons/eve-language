module Primitives(primitiveEnv) where
import Data
import Utils
import Control.Monad.Error

makePrimitives = zip names $ map (uncurry Primitive) (zip names values)
  where 
    (names, values) = unzip primitives

primitiveEnv = makePrimitives


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

  ("\\&", concat'),
  ("type", typeOf),
  ("len", len),

  ("Int", makeInt),
  ("Bool", makeBool),
  ("Str", makeString),
  ("Sym", makeSymbol),
  ("Tuple", makeList Tuple),
  ("Record", typeObject),
  ("Function", typeObject)]

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

typeOf [Int _] = return $ Primitive "Int" makeInt
typeOf [Bool _] = return $ Primitive "Bool" makeBool
typeOf [String _] = return $ Primitive "Str" makeString
typeOf [Symbol _] = return $ Primitive "Sym" makeSymbol
typeOf [Tuple _] = return $ Primitive "Tuple" $ makeList Tuple
typeOf [Record _] = return $ Primitive "Record" typeObject
typeOf [Function _ _ _] = return $ Primitive "Function" typeObject
typeOf [Primitive _ _] = return $ Primitive "Function" typeObject
typeOf [MultiMethod _] = return $ Primitive "Function" typeObject

len [String xs] = return . Int $ length xs
len [Tuple xs] = return . Int $ length xs
len [Record xs] = return . Int $ length xs
len _ = typeError "Length requires a sequence or container"

concat' [String xs, String ys] = return $ String (xs ++ ys)
concat' [Tuple xs, Tuple ys] = return $ Tuple (xs ++ ys)
concat' _ = typeError "Concatenation needs a sequence"

typeObject _ = typeError "Pure type objects cannot be applied."

makeInt [Int x] = return . Int $ x
makeInt [Bool x] = return . Int $ if x then 1 else 0
makeInt [String x] = return . Int $ read x
makeInt _ = typeError "Int expects a single Int, Bool, or String"

makeBool [Int x] = return . Bool $ x /= 0
makeBool [Bool x] = return . Bool $ x
makeBool [String x] = return . Bool $ length x > 0
makeBool [Tuple xs] = return . Bool $ length xs > 0
makeBool _ = typeError "Bool expects a single Int, Bool, String, Tuple, or List"

makeString [x] = return . String $ show x
makeString _ = typeError "String expects a single argument"

makeSymbol [String x] = return $ Symbol x
makeSymbol _ = typeError "Sym expects a string"

makeList constr [Tuple xs] = return $ constr xs
makeList constr [x] = return $ constr [x]
makeList constr xs = return $ constr xs
