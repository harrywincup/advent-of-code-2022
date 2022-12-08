module Day8.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Array as A
import Data.Maybe as M
import Data.Int as Int

--

runA :: Effect Unit
runA = 
    buildForest
    <#> countVisibleTrees
    >>= (show >>> log)

runB :: Effect Unit
runB =
    buildForest
    <#> calculateHighestScenicScore
    >>= (show >>> log)
--

type Tree =
    { x :: Int
    , y :: Int
    , height :: Int
    }

type SurroundingTrees = 
    { north :: Array Tree
    , east :: Array Tree
    , south :: Array Tree
    , west :: Array Tree
    }

buildForest :: Effect (Array Tree)
buildForest = 
    readTextFile UTF8 "./src/Day8/input.txt"
    <#> S.split (S.Pattern "\n")
    <#> A.dropEnd 1
    <#> A.mapWithIndex buildRow
    <#> A.concat


buildRow :: Int -> String -> Array Tree
buildRow yCoord row = 
    row
    # SCU.toCharArray
    # A.mapWithIndex (buildTree yCoord)

buildTree :: Int -> Int -> Char -> Tree
buildTree yCoord xCoord heightChar =
    let
        height =
            heightChar
            # SCU.singleton
            # Int.fromString
            # M.fromMaybe 0
     in
     { x: xCoord
     , y: yCoord
     , height: height
     } 

countVisibleTrees :: Array Tree -> Int
countVisibleTrees forest =
    forest
        # A.filter (isTreeVisible forest)
        # A.length

calculateHighestScenicScore :: Array Tree -> Int
calculateHighestScenicScore forest =
    forest
        # map (calculateScenicScoreForTree forest)
        # A.sort
        # A.last
        # M.fromMaybe 0

calculateScenicScoreForTree :: Array Tree -> Tree -> Int
calculateScenicScoreForTree forest tree =
    let
        sts = findSurroundingTrees forest tree

        viewingDistanceN = calculateViewingDistanceFromTree 0 (sts.north # A.sortWith (_.y) # A.reverse) tree
        viewingDistanceE = calculateViewingDistanceFromTree 0 (sts.east # A.sortWith (_.x)) tree
        viewingDistanceS = calculateViewingDistanceFromTree 0 (sts.south # A.sortWith (_.y)) tree
        viewingDistanceW = calculateViewingDistanceFromTree 0 (sts.west # A.sortWith (_.x) # A.reverse) tree
    in
    viewingDistanceN * viewingDistanceE * viewingDistanceS * viewingDistanceW


calculateViewingDistanceFromTree :: Int -> Array Tree -> Tree -> Int
calculateViewingDistanceFromTree d surroundingTrees tree =
    case A.uncons surroundingTrees of 
         M.Nothing -> d
         M.Just { head: x, tail: xs } -> 
             case x.height of
                  h
                      | h >= tree.height ->  d + 1
                      | h < tree.height -> calculateViewingDistanceFromTree (d + 1) xs tree
                      | otherwise -> d



isTreeVisible :: Array Tree -> Tree -> Boolean
isTreeVisible forest tree =
    let
        sts = findSurroundingTrees forest tree
        treeIsOnPerimeter = ([sts.north, sts.east, sts.south, sts.west] # A.filter A.null # not A.null)

        blockingTrees = 
            [sts.north, sts.east, sts.south, sts.west]
                # map (A.filter (isTreeAsTall tree) >>> A.length)

        treeIsBlocked = A.notElem 0 blockingTrees
     in
     treeIsOnPerimeter || (not treeIsOnPerimeter && not treeIsBlocked)

isTreeAsTall :: Tree -> Tree -> Boolean
isTreeAsTall t1 t2 =
    t2.height >= t1.height

findSurroundingTrees :: Array Tree -> Tree -> SurroundingTrees
findSurroundingTrees forest tree =
    { north: forest # A.filter (\t -> t.x == tree.x && t.y < tree.y)
    , east:  forest # A.filter (\t -> t.x > tree.x && t.y == tree.y)
    , south: forest # A.filter (\t -> t.x == tree.x && t.y > tree.y)
    , west:  forest # A.filter (\t -> t.x < tree.x && t.y == tree.y)
    }



