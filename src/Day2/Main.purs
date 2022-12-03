module Day2.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Foldable (sum)
import Data.Tuple (Tuple(..))

--

runA :: Effect Unit
runA = run calculateScoreForTurnV1

runB :: Effect Unit
runB = run calculateScoreForTurnV2

run :: (Array String -> Int) -> Effect Unit
run calculationFn =
    buildTurns
        <#> map calculationFn
        <#> sum
        <#> show
        >>= log

--

buildTurns :: Effect (Array (Array String))
buildTurns = 
    readTextFile UTF8 "./src/Day2/input.txt"
    <#> String.split (String.Pattern "\n")
    <#> map (String.split (String.Pattern " "))


data Shape 
    = Rock 
    | Paper 
    | Scissors 

derive instance eqShape :: Eq Shape
instance ordShape :: Ord Shape where
    compare = case _, _ of 
        Rock, Scissors -> GT
        Paper, Rock -> GT
        Scissors, Paper -> GT
        _, _ -> LT

data Outcome 
    = Win
    | Draw
    | Lose

calculateScoreForTurnV1 :: Array String -> Int
calculateScoreForTurnV1 codes = 
    case codes of 
        [opponent, player] ->
           calculateScoreForTurn (getShapeForCode(opponent)) (getShapeForCode(player))

        _ -> 0


calculateScoreForTurnV2 :: Array String -> Int
calculateScoreForTurnV2 codes = 
    case codes of 
        [a, b] -> do
           let opponentShape =  getShapeForCode a
           let desiredOutcome = getOutcomeForCode b
           let playerShape = pickShapeToForceOutcome opponentShape desiredOutcome

           calculateScoreForTurn opponentShape playerShape

        _ -> 0

calculateScoreForTurn :: Maybe Shape -> Maybe Shape -> Int
calculateScoreForTurn opponent player =
    determineOutcome opponent player
        # getScoreForOutcome
        # (+) (getScoreForShape player)


getShapeForCode :: String -> Maybe Shape
getShapeForCode c =
    case c of
        "A" -> Just Rock
        "X" -> Just Rock
        "B" -> Just Paper
        "Y" -> Just Paper
        "C" -> Just Scissors
        "Z" -> Just Scissors
        _ -> Nothing

getOutcomeForCode :: String -> Maybe Outcome
getOutcomeForCode code =
    case code of
        "X" -> Just Lose
        "Y" -> Just Draw
        "Z" -> Just Win
        _ -> Nothing


determineOutcome :: Maybe Shape -> Maybe Shape -> Maybe Outcome
determineOutcome opponent player 
    | player == opponent = Just Draw
    | player < opponent = Just Lose
    | player > opponent = Just Win
    | otherwise = Nothing

getShapeThatBeats :: Maybe Shape -> Maybe Shape 
getShapeThatBeats = case _ of 
    Nothing -> Nothing
    Just Rock -> Just Paper
    Just Paper -> Just Scissors
    Just Scissors -> Just Rock

pickShapeToForceOutcome :: Maybe Shape -> Maybe Outcome -> Maybe Shape
pickShapeToForceOutcome opponentShape desiredOutcome =
    case desiredOutcome of 
         Nothing -> Nothing
         Just Win -> getShapeThatBeats opponentShape
         Just Draw -> opponentShape
         Just Lose -> (getShapeThatBeats >>> getShapeThatBeats) opponentShape

getScoreForOutcome :: Maybe Outcome -> Int
getScoreForOutcome o =
    case o of
        (Just Win) -> 6
        (Just Draw) -> 3
        _ -> 0

getScoreForShape :: Maybe Shape -> Int
getScoreForShape s = 
    case s of 
        Nothing -> 0
        Just Rock -> 1
        Just Paper -> 2
        Just Scissors -> 3



