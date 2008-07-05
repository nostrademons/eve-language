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
  ("get", get),
  ("iter", iter),
  ("next", iterNext),
  ("has_next", iterHasNext),

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

lenHelper xs = return . Int $ length xs
len [String xs] = lenHelper xs
len [Tuple xs] = lenHelper xs
len [Record xs] = lenHelper xs
len _ = typeError "Length requires a sequence or container"

fixNegative xs index = if index < 0 then length xs + index else index
getHelper xs index = return $ xs !! fixNegative xs index
sliceHelper constr xs fields = do
    start <- lookupField "start"
    end <- lookupField "stop"
    return . constr $ take (end - start) (drop start xs)
  where
    extract (Int num) = return $ fixNegative xs num
    extract _ = typeError "Index is not an integer"
    lookupField field = maybe (typeError $ "No " ++ field ++ " field") extract $ lookup field fields

get [Int index, String xs] = getHelper xs index >>= \c -> return (String [c])
get [Int index, Tuple xs] = getHelper xs index
get [String index, Record xs] = 
    maybe (typeError $ "Unknown field " ++ index) return $ lookup index xs
get [Record fields, String xs] = sliceHelper String xs fields
get [Record fields, Tuple xs] = sliceHelper Tuple xs fields
get [Tuple xs] = getHelper xs 0
get [SequenceIter (String xs) index] = return $ String [xs !! index]
get [SequenceIter (Tuple xs) index] = return $ xs !! index
get [RecordIter (Record xs) index] = return $ Tuple [String $ fst pair, snd pair]
  where pair = xs !! index
get (val:_) = typeError (show val ++ " is not indexable")
get _ = typeError "get requires at least one argument"

iter [val@(String _)] = return $ SequenceIter val 0
iter [val@(Tuple _)] = return $ SequenceIter val 0
iter [val@(Record _)] = return $ RecordIter val 0
iter val = typeError (show val ++ " is not iterable")

-- TODO: these just expose a Haskell error if you overshoot the end of the
-- sequence, instead of raising a nice EveError
iterNext [SequenceIter val index] = return $ SequenceIter val (index + 1)
iterNext [RecordIter val index] = return $ RecordIter val (index + 1)
iterNext _ = typeError "Not an iterator."

iterHasNextHelper xs index = return . Bool $ index < length xs
iterHasNext [SequenceIter (String xs) index] = iterHasNextHelper xs index
iterHasNext [SequenceIter (Tuple xs) index] = iterHasNextHelper xs index
iterHasNext [RecordIter (Record xs) index] = iterHasNextHelper xs index
iterHasNext _ = typeError "Not an iterator."

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
