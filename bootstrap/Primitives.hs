module Primitives(makeInt, makeBool, makeString, makeSymbol, makeTuple, makeRecord, 
        makePrimitive, makeFunction, makePrimitives, makeNone, primitives) where
import Data
import Utils
import Control.Monad.Error hiding (join)

makePrimitives primitives = zip names $ map makePrimitive (zip names values)
  where 
    (names, values) = unzip primitives

makeMethods = map addMethodSelf . makePrimitives
  where
    addMethodSelf (var, Primitive name fn fields) = 
        (var, Primitive name fn (("method_self", makeNone) : fields))

-- TODO: figure out how to include the unbound methods on the contructor function, 
-- but bound methods in the prototype.

-- Individual method lists for different types

numberPrimitives = makePrimitives [
  ("pow", numericBinop ((^) . fromIntegral)),
  ("mul", numericBinop (*)),
  ("div", numericBinop div),
  ("mod", numericBinop mod),
  ("add", numericBinop (+)),
  ("sub", numericBinop (-))]

-- TODO: Eq should be a standalone function instead of an instance method, so
-- it works on plain records
eqPrimitives = makePrimitives [
  ("eq", eqBinop (==)),
  ("ne", eqBinop (/=))]

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
  ("is_valid", iterIsValid)]

sequencePrimitives = makePrimitives [
  ("add", concat'),
  ("mul", repeat'),
  ("iter", iter),
  ("len", len),
  ("get", get),
  ("slice", slice)]

-- Type objects, serving as constructor functions, type flags, and prototypes
baseProto = Record $ ("proto", (Bool False [])) : eqPrimitives
makeTypeObj name constr methods = Primitive name constr $ ("proto", baseProto) : concat methods
intProto = makeTypeObj "Int" convertToInt [numberPrimitives, eqPrimitives, orderedPrimitives]
boolProto = makeTypeObj "Bool" convertToBool [eqPrimitives]
strProto = makeTypeObj "Str" convertToString [eqPrimitives, orderedPrimitives, 
    sequencePrimitives]
symProto = makeTypeObj "Sym" convertToSymbol [eqPrimitives]
tupleProto = makeTypeObj "Tuple" typeObject [eqPrimitives, sequencePrimitives]
recordProto = makeTypeObj "Record" typeObject [eqPrimitives]
primitiveProto = makeTypeObj "Primitive" typeObject [eqPrimitives]
functionProto = makeTypeObj "Function" typeObject [eqPrimitives]
makeNone = makeTypeObj "None" typeObject [eqPrimitives]

-- Global primitives.  Augmented in Eval by the primitives that need access to apply
primitives = makePrimitives [
    ("dump", dump),
    ("vars", vars),
    ("locals", locals)] ++ boolPrimitives ++ map bindPrimitive 
        -- "Record" and "Tuple" are added by the evaluator, since the require
        -- access to the iterator machinery.  They rely on the fact that
        -- primitives with equal names are equal
        [intProto, boolProto, strProto, symProto, primitiveProto, functionProto]
  where
    bindPrimitive val@(Primitive name _ _) = (name, val)

-- Factories that bundle a basic data type up with the primitives associated
-- with it

makeInt val = Int val [("proto", intProto), ("method_receiver", makeNone)]
makeBool val = Bool val [("proto", boolProto), ("method_receiver", makeNone)]
makeString val = String val [("proto", strProto), ("method_receiver", makeNone)]
makeSymbol val = Symbol val [("proto", symProto)]
makeTuple val = Tuple val [("proto", tupleProto), ("method_receiver", makeNone)]
makeRecord val = Record $ [("proto", recordProto)] ++ val
makePrimitive (name, fn) = Primitive name fn [("proto", primitiveProto)]
makeFunction argData isShown pos body env = Function argData isShown pos body env [("proto", functionProto)]

typeError = throwEveError . TypeError

numericBinop :: (Int -> Int -> Int) -> [EveData] -> EveM EveData
numericBinop f [Int arg1 _, Int arg2 _] = return $ makeInt $ f arg1 arg2
numericBinop f _ = typeError "Numeric operator expects 2 ints"

eqBinop f [val1, val2] = return $ makeBool $ f val1 val2
eqBinop f _ = typeError "Equality testing takes two arguments"

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
typeOf [Tuple _ _] = return $ makePrimitive ("Tuple", typeObject)
typeOf [Record _] = return $ makePrimitive ("Record", typeObject)
typeOf [Function _ _ _ _ _ _] = return $ makePrimitive ("Function", typeObject)
typeOf [Primitive _ _ _] = return $ makePrimitive ("Function", typeObject)

lenHelper xs = return . makeInt $ length xs
len :: [EveData] -> EveM EveData
len [String xs _] = lenHelper xs
len [Tuple xs _] = lenHelper xs
len [Record xs] = lenHelper $ recordFields xs
len _ = typeError "Length requires a sequence or container"

fixNegative xs index = if index < 0 then length xs + index else index
getHelper xs index = return $ xs !! fixNegative xs index
sliceHelper constr xs rawStart rawEnd = return . constr $ take (end - start) (drop start xs)
  where
    start = fixNegative xs rawStart
    end = fixNegative xs rawEnd

get [String xs _, Int index _] = getHelper xs index >>= return . makeString . (: [])
get [Tuple xs _, Int index _] = getHelper xs index
get [Record xs, String index _] = 
    maybe (typeError $ "Unknown field " ++ index) return $ lookup index xs
get [Tuple xs _] = getHelper xs 0
get [SequenceIter (String xs _) index _] = return $ makeString [xs !! index]
get [SequenceIter (Tuple xs _) index _] = return $ xs !! index
get [RecordIter (Record xs) index _] = return $ makeTuple [makeString $ fst pair, snd pair]
  where pair = xs !! index
get (val:_) = typeError (show val ++ " is not indexable")
get _ = typeError "get requires at least one argument"

slice [String xs _, Int start _, Int end _] = sliceHelper makeString xs start end
slice [Tuple xs _, Int start _, Int end _] = sliceHelper makeTuple xs start end
slice _ = typeError "slice requires a sequence, an integer start, and an integer end"

allIterPrimitives = concat [eqPrimitives, iterPrimitives]
makeIter constr val = do
    iterProto <- lookupEnv "Iterator" `catchError` (const $ return makeNone)
    return $ constr val 0 $ [("proto", iterProto), ("method_receiver", makeNone)] ++ allIterPrimitives
iter [val@(String _ _)] = makeIter SequenceIter val
iter [val@(Tuple _ _)] = makeIter SequenceIter val
iter [val@(SequenceIter _ _ _)] = return val
iter [val@(RecordIter _ _ _)] = return val
iter val = typeError (show val ++ " is not iterable")

vars [val@(Record fields)] = makeIter RecordIter $ Record $ recordFields fields

-- TODO: these just expose a Haskell error if you overshoot the end of the
-- sequence, instead of raising a nice EveError
iterNext [SequenceIter val index fields] = return $ SequenceIter val (index + 1) fields
iterNext [RecordIter val index fields] = return $ RecordIter val (index + 1) fields
iterNext _ = typeError "Not an iterator."

iterHasNextHelper xs index = return . makeBool $ index < length xs
iterIsValid [SequenceIter (String xs _) index _] = iterHasNextHelper xs index
iterIsValid [SequenceIter (Tuple xs _) index _] = iterHasNextHelper xs index
iterIsValid [RecordIter (Record xs) index _] = iterHasNextHelper xs index
iterIsValid _ = typeError "Not an iterator."

concat' [String xs fields, String ys _] = return $ setAttributes (makeString (xs ++ ys)) fields
concat' [Tuple xs fields, Tuple ys _] = return $ setAttributes (makeTuple (xs ++ ys)) fields
concat' [Record xs, Record ys] = return $ makeRecord (xs ++ ys)
concat' _ = typeError "Concatenation needs a sequence"

repeat' [String xs _, Int num _] = return . makeString . concat $ replicate num xs
repeat' [Tuple xs _, Int num _] = return . makeTuple . concat $ replicate num xs
repeat' [val@(Record _), Int num _] = return val
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

dump [val] = return $ makeString (show val ++ "\n" ++ showProtoChain val)
  where 
    showProtoChain (Bool False []) = ""
    showProtoChain obj = join "\n" (map showFields $ attributes obj) 
                                ++ "\n\n" ++ showProtoChain (prototype obj)

locals [] = frameVars >>= return . makeRecord
