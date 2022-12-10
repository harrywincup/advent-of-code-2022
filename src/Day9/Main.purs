module Day9.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as S
import Data.Array as A
import Data.Maybe as M
import Data.Function as Fn
import Data.Int as I
import Data.Ord as Ord
import Data.Map as Map

--

run :: Int -> Effect Unit
run ropeLength = 
    readTextFile UTF8 "./src/Day9/input.txt"
    <#> S.split (S.Pattern "\n")
    <#> A.dropEnd 1
    <#> A.foldl moveRope (A.replicate ropeLength { x: 0, y: 0, visits: Map.singleton "0::0" 1 })
    <#> A.head
    <#> map (_.visits >>> Map.size)
    >>= (show >>> log)

runA :: Effect Unit
runA = run 2

runB :: Effect Unit
runB = run 10

--


type Rope = Array Knot
type Knot = 
    { x :: Int
    , y :: Int
    , visits :: Map.Map String Int
    }

type Delta = { dx :: Int, dy :: Int }

moveRope :: Rope -> String -> Rope
moveRope rope m = do
    let 
        parts = S.split (S.Pattern " ") m
        direction = A.index parts 0 

        steps :: Int
        steps = M.fromMaybe 0 $ join $ I.fromString <$> A.index parts 1

        delta :: Delta
        delta = case direction of
                     M.Just "U" -> { dx: 0, dy: (-1) }
                     M.Just "D" -> { dx: 0, dy: 1 }
                     M.Just "L" -> { dx: (-1), dy: 0 }
                     M.Just "R" -> { dx: 1, dy: 0}
                     _          -> { dx: 0, dy: 0}

    Fn.applyN (moveSection (A.length rope - 1) delta) steps rope

moveSection :: Int -> Delta -> Rope -> Rope
moveSection knotIndex delta rope =
    case knotIndex of 
         i | i < 0 -> rope -- no more rope
           | i == (A.length rope - 1) -> do -- move head knot
                let newHead = (A.modifyAt (knotIndex) (moveHead delta) >>> M.fromMaybe rope) rope
                moveSection (knotIndex - 1) delta newHead
                    
           | otherwise -> do -- move non-head knot
                let lead = A.index rope (knotIndex + 1)
                let newRope = case lead of 
                        M.Nothing -> rope
                        M.Just l -> A.modifyAt knotIndex (moveChaser l) rope # M.fromMaybe rope

                moveSection (knotIndex - 1) delta newRope

moveHead :: Delta -> Knot -> Knot
moveHead delta head =
    head { x = head.x + delta.dx, y = head.y + delta.dy }

moveChaser :: Knot -> Knot -> Knot
moveChaser lead chaser = do
   let 
       distX = lead.x - chaser.x
       distY = lead.y - chaser.y
       shouldMoveChaser = (Ord.abs distX) > 1 || (Ord.abs distY) > 1
       chaserX = if shouldMoveChaser && distX /= 0 then chaser.x + (Ord.signum distX) else chaser.x
       chaserY = if shouldMoveChaser && distY /= 0 then chaser.y + (Ord.signum distY) else chaser.y
       chaserVisits = if shouldMoveChaser then Map.insert (show chaserX <> "::" <> show chaserY) 1 chaser.visits else chaser.visits

   chaser { x = chaserX, y = chaserY, visits = chaserVisits }
