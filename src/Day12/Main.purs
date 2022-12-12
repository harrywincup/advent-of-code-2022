module Day12.Main where

import Prelude

import Debug (spy)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Char (toCharCode)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Array as A
import Data.List as L
import Data.Maybe as M
import Data.Int as I
import Data.Tuple as T
import Data.Foldable (sum, product)
import Data.Graph as G

--

runA :: Effect Unit
runA = 
    buildGrid
    <#> spy "grid"
    <#> buildGraph
    <#> G.shortestPath 'S' 'E'
    <#> spy "shortest path"
    --<#> map (L.length)
    >>= (show >>> log)

--

type Tile =
    { x :: Int
    , y :: Int
    , c :: Char
    , h :: Int
    }
     
type Node = T.Tuple Char (L.List Edge)
type Edge = T.Tuple Char Int

buildGrid :: Effect (Array Tile)
buildGrid = 
    readTextFile UTF8 "./src/Day12/test-input.txt"
    <#> S.split (S.Pattern "\n")
    <#> A.dropEnd 1
    <#> A.mapWithIndex buildRow
    <#> A.concat

buildRow :: Int -> String -> Array Tile
buildRow yCoord row = 
    row
    # SCU.toCharArray
    # A.mapWithIndex (buildTile yCoord)

buildTile :: Int -> Int -> Char -> Tile
buildTile yCoord xCoord heightChar =
     { c: heightChar 
     , x: xCoord
     , y: yCoord
     , h: getHeightForTile heightChar
     } 

getHeightForTile :: Char -> Int
getHeightForTile c = do
    let
        hc = case c of 
                  'S' -> 'a'
                  'E' -> 'z'
                  _ -> c
    
    toCharCode hc

buildGraph :: Array Tile -> G.Graph Char Int
buildGraph = (buildAdjacencyList >>> G.fromAdjacencyList)

buildAdjacencyList :: Array Tile -> G.AdjacencyList Char Int
buildAdjacencyList ts = map (buildNode ts) ts # L.fromFoldable

buildNode :: Array Tile -> Tile -> Node
buildNode tiles tile = do
    let 
        n = A.find (\t -> t.x == tile.x && t.y == tile.y - 1) tiles
        e = A.find (\t -> t.x == tile.x + 1 && t.y == tile.y) tiles
        s = A.find (\t -> t.x == tile.x && t.y == tile.y + 1) tiles
        w = A.find (\t -> t.x == tile.x - 1 && t.y == tile.y) tiles

        neighbors = A.filter (\t -> t.h - tile.h <= 1) $ A.catMaybes [n,e,s,w]

        node = spy "node" $ T.Tuple tile.c (L.fromFoldable (neighbors # map (tileToEdge tile.h)))

    node
    

tileToEdge :: Int -> Tile -> Edge
tileToEdge th t = do
    T.Tuple t.c (t.h - th)





    
