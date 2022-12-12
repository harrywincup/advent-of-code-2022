module Day11.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as S
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Maybe as M
import Data.Either as E
import Data.Int as I
import Data.Decimal as D
import Data.Ord as Ord
import Data.Foldable (sum, product)
import Data.Traversable (sequence, traverse)
import Data.Bifunctor as B
import Parsing as P
import Parsing.String as PS
import Parsing.String.Basic as PSB
import Parsing.Combinators as PC
import Parsing.Combinators.Array as PCA
import Record.Extra (sequenceRecord)

--

runA :: Effect Unit
runA = 
    readTextFile UTF8 "./src/Day11/input.txt"
    <#> S.split (S.Pattern "\n")
    <#> A.groupBy (\a b -> not S.null a && not S.null b)
    <#> map NEA.toArray
    <#> A.filter (A.head >>> M.fromMaybe "" >>> (/=) "")
    <#> traverse defineMonkey
    <#> map (performRounds 20)
    <#> map (map _.inspections >>> A.sort >>> A.takeEnd 2)
    <#> map (product)
    >>= (show >>> log)

--

data Operand 
    = New Int
    | Old

instance showOperand :: Show Operand where
    show (New n) = "New " <> show n
    show Old = "Old"

data Operation 
    = Add Operand
    | Multiply Operand

instance showOperation :: Show Operation where
    show (Add x) = "add " <> show x
    show (Multiply x) = "multiply " <> show x

type Monkey =
    { id :: Int
    , items :: Array Int
    , operation :: Operation
    , divisibleBy :: Int
    , receiverOnTrue :: Int
    , receiverOnFalse :: Int
    , inspections :: Int
    }

defineMonkey ::  Array String -> E.Either String Monkey
defineMonkey attrs =
    let 
        attrParsers = 
            { id: P.runParser (A.index attrs 0 # M.fromMaybe "") idParser
            , items: P.runParser (A.index attrs 1 # M.fromMaybe "") itemsParser
            , operation: P.runParser (A.index attrs 2 # M.fromMaybe "") operationParser
            , divisibleBy: P.runParser (A.index attrs 3 # M.fromMaybe "") conditionParser
            , receiverOnTrue: P.runParser (A.index attrs 4 # M.fromMaybe "") receiverTrueParser
            , receiverOnFalse: P.runParser (A.index attrs 5 # M.fromMaybe "") receiverFalseParser
            , inspections: E.Right 0
            }
     in
     sequenceRecord attrParsers
     # B.lmap P.parseErrorMessage

performRounds :: Int -> Array Monkey -> Array Monkey
performRounds rounds monkeys = do
    case rounds of 
         0 -> monkeys
         r -> performRounds (r - 1) (A.foldl doShenanigans monkeys monkeys)

doShenanigans :: Array Monkey -> Monkey -> Array Monkey
doShenanigans monkeys monkey = do
    monkeys
        # A.find (\{id} -> id == monkey.id)
        # M.fromMaybe monkey
        # _.items
        # A.foldl (moveItem monkey.id) monkeys

moveItem :: Int -> Array Monkey -> Int -> Array Monkey
moveItem id monkeys i = do
    let
        maybeInspector = A.find (\m -> m.id == id) monkeys
        
        updatedMonkeys = 
            case maybeInspector of 
                M.Nothing -> monkeys
                M.Just inspector -> do
                    let
                        preWorry = applyOperation inspector.operation i
                        postWorry = preWorry `div` 3
                        receiverID = determineReceiverID postWorry inspector
                        receiver = A.find (\{id} -> id == receiverID) monkeys 
                        withReceiver = 
                            case receiver of 
                                 M.Just r -> 
                                    monkeys
                                       # A.updateAt r.id (r { items = A.snoc r.items postWorry })
                                       # M.fromMaybe monkeys 
                                 M.Nothing -> monkeys

                        withInspector =
                            A.updateAt inspector.id (inspector { inspections = inspector.inspections + 1, items = A.drop 1 inspector.items }) withReceiver
                            # M.fromMaybe withReceiver

                    withInspector

    updatedMonkeys

determineReceiverID :: Int -> Monkey -> Int
determineReceiverID i monkey =
    case (D.fromInt i) `div` (D.fromInt monkey.divisibleBy) of
         d | D.isInteger d -> monkey.receiverOnTrue
           | otherwise -> monkey.receiverOnFalse

applyOperation :: Operation -> Int -> Int
applyOperation o i =
    case o of 
         Add op -> 
             case op of 
                  New n -> i + n
                  Old -> i + i

         Multiply op ->
             case op of 
                  New n -> i * n
                  Old -> i * i

idParser :: P.Parser String Int
idParser = do
    _ <- PS.string "Monkey"
    _ <- PSB.space
    i <- PSB.intDecimal

    pure $ i

itemsParser :: P.Parser String (Array Int)
itemsParser = do
    _ <- PSB.whiteSpace
    _ <- PS.string "Starting items:"
    _ <- PSB.space
    items <- PCA.many do
       n <- PSB.intDecimal
       _ <- PC.optional (PS.string ", ")

       pure $ n

    pure items

operationParser :: P.Parser String Operation    
operationParser = do
    _ <- PSB.whiteSpace
    _ <- PS.string "Operation: new = old"
    _ <- PSB.space
    o <- (PS.char '*' $> Multiply) PC.<|> (PS.char '+' $> Add)
    _ <- PSB.space
    n <- (PSB.intDecimal <#> New)  PC.<|> (PS.string "old" $> Old)

    pure (o n)

conditionParser :: P.Parser String Int 
conditionParser = do
    _ <- PSB.whiteSpace
    _ <- PS.string "Test: divisible by"
    _ <- PSB.space
    n <- PSB.intDecimal

    pure n 

receiverTrueParser :: P.Parser String Int
receiverTrueParser = do
    _ <- PSB.whiteSpace
    _ <- PS.string "If true: throw to monkey"
    _ <- PSB.space
    n <- PSB.intDecimal

    pure n

receiverFalseParser :: P.Parser String Int
receiverFalseParser = do
    _ <- PSB.whiteSpace
    _ <- PS.string "If false: throw to monkey"
    _ <- PSB.space
    n <- PSB.intDecimal

    pure n


