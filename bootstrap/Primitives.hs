module Primitives(makeInt, makeBool, makeString, makeSymbol, 
    makeTuple, makeRecord, makePrimitive, makeFunction, makePrimitives, primitiveEnv) where
import Data
import Utils
import Control.Monad.Error

makePrimitives primitives = zip names $ map makePrimitive (zip names values)
  where 
    (names, values) = unzip primitives

-- Individual method lists for different types

numberPrimitives = makePrimitives [
  ("pow", numericBinop ((^) . fromIntegral)),
  ("mul", numericBinop (*)),
  ("div", numericBinop div),
  ("mod", numericBinop mod),
  ("add", numericBinop (+)),
  ("sub", numericBinop (-))]

eqPrimitives = makePrimitives [
  ("eq", numBoolBinop (==)),
  ("ne", numBoolBinop (/=))]

orderedPrimitives = makePrimitives [
  ("lt", numBoolBinop (<)),
  ("gt", numBoolBinop (>)),
  ("ge", numBoolBinop (>=)),
  ("le", numBoolBinop (<=))]

boolPrimitives = makePrimitives [
  ("and_", boolBoolBinop (&&)),
  ("or_", boolBoolBinop (||)),
  ("not_", boolOp (not))]

iterPrimitives = makePrimitives [
  ("iter", iter),
  ("get", get),
  ("next", iterNext),
  ("has_next", iterHasNext)]

sequencePrimitives = makePrimitives [
  ("add", concat'),
  ("mul", repeat'),
  ("iter", iter),
  ("len", len),
  ("get", get)]

-- Type objects, serving as constructor functions, type flags, and prototypes
baseProto = Record $ ("proto", (Bool False [])) : eqPrimitives
makeTypeObj name constr methods = Primitive name constr $ ("proto", baseProto) : concat methods
intProto = makeTypeObj "Int" convertToInt [numberPrimitives, eqPrimitives, orderedPrimitives]
boolProto = makeTypeObj "Bool" convertToBool [eqPrimitives]
strProto = makeTypeObj "Str" convertToString [eqPrimitives, orderedPrimitives, sequencePrimitives]
symProto = makeTypeObj "Sym" convertToSymbol [eqPrimitives]
tupleProto = makeTypeObj "Tuple" convertToTuple [eqPrimitives, sequencePrimitives]
recordProto = makeTypeObj "Record" typeObject [eqPrimitives]
primitiveProto = makeTypeObj "Primitive" typeObject [eqPrimitives]
functionProto = makeTypeObj "Function" typeObject [eqPrimitives]

-- Global primitives.  Augmented in Eval by the primitives that need access to apply
primitiveEnv = boolPrimitives ++ map bindPrimitive 
    -- "Record" is added by the evaluator and relies on the fact that primitives with
    -- equal names are equal
    [intProto, boolProto, strProto, symProto, tupleProto, primitiveProto, functionProto]
  where
    bindPrimitive val@(Primitive name _ _) = (name, val)

-- Factories that bundle a basic data type up with the primitives associated
-- with it

makeInt val = Int val [("proto", intProto)]
makeBool val = Bool val [("proto", boolProto)]
makeString val = String val [("proto", strProto)]
makeSymbol val = Symbol val [("proto", symProto)]
makeTuple val = Tuple val [("proto", tupleProto)]
makeRecord val = Record $ ("proto", recordProto) : val
makePrimitive (name, fn) = Primitive name fn [("proto", primitiveProto)]
makeFunction argData pos body env = Function argData pos body env [("proto", functionProto)]

typeError = throwEveError . TypeError

numericBinop :: (Int -> Int -> Int) -> [EveData] -> EveM EveData
numericBinop f [Int arg1 _, Int arg2 _] = return $ makeInt $ f arg1 arg2
numericBinop f _ = typeError "Numeric operator expects 2 ints"

numBoolBinop f [Int arg1 _, Int arg2 _] = return $ makeBool $ f arg1 arg2
numBoolBinop f _ = typeError "Boolean operator expects 2 ints"

boolBoolBinop f [Bool arg1 _, Bool arg2 _] = return $ makeBool $ f arg1 arg2
boolBoolBinop f _ = typeError "Boolean operator expects 2 bools"

boolOp f [Bool arg _] = return $ makeBool $ f arg
boolOp f _ = typeError "Expects a boolean"

typeOf [Int _ _] = return $ makePrimitive ("Int", convertToInt)
typeOf [Bool _ _] = return $ makePrimitive ("Bool", convertToBool)
typeOf [String _ _] = return $ makePrimitive ("Str", convertToString)
typeOf [Symbol _ _] = return $ makePrimitive ("Sym", convertToSymbol)
typeOf [Tuple _ _] = return $ makePrimitive ("Tuple", convertToTuple)
typeOf [Record _] = return $ makePrimitive ("Record", typeObject)
typeOf [Function _ _ _ _ _] = return $ makePrimitive ("Function", typeObject)
typeOf [Primitive _ _ _] = return $ makePrimitive ("Function", typeObject)

lenHelper xs = return . makeInt $ length xs
len :: [EveData] -> EveM EveData
len [String xs _] = lenHelper xs
len [Tuple xs _] = lenHelper xs
len [Record xs] = lenHelper $ recordFields xs
len _ = typeError "Length requires a sequence or container"

fixNegative xs index = if index < 0 then length xs + index else index
getHelper xs index = return $ xs !! fixNegative xs index
sliceHelper constr xs fields = do
    start <- lookupField "start"
    end <- lookupField "stop"
    return . constr $ take (end - start) (drop start xs)
  where
    extract (Int num _) = return $ fixNegative xs num
    extract _ = typeError "Index is not an integer"
    lookupField field = maybe (typeError $ "No " ++ field ++ " field") extract $ lookup field fields

get [String xs _, Int index _] = getHelper xs index >>= \c -> return (makeString [c])
get [Tuple xs _, Int index _] = getHelper xs index
get [Record xs, String index _] = 
    maybe (typeError $ "Unknown field " ++ index) return $ lookup index xs
get [String xs _, Record fields] = sliceHelper makeString xs fields
get [Tuple xs _, Record fields] = sliceHelper makeTuple xs fields
get [Tuple xs _] = getHelper xs 0
get [SequenceIter (String xs _) index _] = return $ makeString [xs !! index]
get [SequenceIter (Tuple xs _) index _] = return $ xs !! index
get [RecordIter (Record xs) index _] = return $ makeTuple [makeString $ fst pair, snd pair]
  where pair = xs !! index
get (val:_) = typeError (show val ++ " is not indexable")
get _ = typeError "get requires at least one argument"

allIterPrimitives = concat [eqPrimitives, iterPrimitives]
makeIter constr val = do
    env <- getEnv
    return $ constr val 0 $ ("proto", findIterator env) : allIterPrimitives
  where
    findIterator env = maybe (error "Iterator prototype not loaded") id $ lookup "Iterator" env
iter [val@(String _ _)] = makeIter SequenceIter val
iter [val@(Tuple _ _)] = makeIter SequenceIter val
iter [val@(Record _)] = makeIter RecordIter val
iter [val@(SequenceIter _ _ _)] = return val
iter [val@(RecordIter _ _ _)] = return val
iter val = typeError (show val ++ " is not iterable")

-- TODO: these just expose a Haskell error if you overshoot the end of the
-- sequence, instead of raising a nice EveError
iterNext [SequenceIter val index fields] = return $ SequenceIter val (index + 1) fields
iterNext [RecordIter val index fields] = return $ RecordIter val (index + 1) fields
iterNext _ = typeError "Not an iterator."

iterHasNextHelper xs index = return . makeBool $ index < length xs
iterHasNext [SequenceIter (String xs _) index _] = iterHasNextHelper xs index
iterHasNext [SequenceIter (Tuple xs _) index _] = iterHasNextHelper xs index
iterHasNext [RecordIter (Record xs) index _] = iterHasNextHelper xs index
iterHasNext _ = typeError "Not an iterator."

concat' [String xs _, String ys _] = return $ makeString (xs ++ ys)
concat' [Tuple xs _, Tuple ys _] = return $ makeTuple (xs ++ ys)
concat' _ = typeError "Concatenation needs a sequence"

repeat' [String xs _, Int num _] = return . makeString . concat $ replicate num xs
repeat' [Tuple xs _, Int num _] = return . makeTuple . concat $ replicate num xs
repeat' _ = typeError "Repetition needs a sequence and an int"

typeObject _ = typeError "Pure type objects cannot be applied."

convertToInt [Int x _] = return . makeInt $ x
convertToInt [Bool x _] = return . makeInt $ if x then 1 else 0
convertToInt [String x _] = return . makeInt $ read x
convertToInt _ = typeError "Int expects a single Int, Bool, or String"

convertToBool [Int x _] = return . makeBool $ x /= 0
convertToBool [Bool x _] = return . makeBool $ x
convertToBool [String x _] = return . makeBool $ length x > 0
convertToBool [Tuple xs _] = return . makeBool $ length xs > 0
convertToBool _ = typeError "Bool expects a single Int, Bool, String, Tuple, or List"

convertToString [x] = return . makeString $ show x
convertToString _ = typeError "String expects a single argument"

convertToSymbol [String x _] = return $ makeSymbol x
convertToSymbol _ = typeError "Sym expects a string"

convertToTuple [Tuple xs _] = return $ makeTuple xs
convertToTuple [x] = return $ makeTuple [x]
convertToTuple xs = return $ makeTuple xs
