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
import Data.Ord as Ord
import Data.Foldable (sum, product)
import Data.Graph as G
import Data.Enum (fromEnum)

--

runA :: Effect Unit
runA = 
    buildGrid
    --<#> spy "grid"
    <#> buildGraph
    <#> G.shortestPath "S020" "E13720"
    --<#> G.shortestPath "S00" "E52"
    <#> spy "shortest path"
    <#> map (L.length)
    --<#> map (_ - 1 )
    >>= (show >>> log)

--

type Tile =
    { x :: Int
    , y :: Int
    , c :: String
--    , h :: Int
    }
     
type Node = T.Tuple String (L.List Edge)
type Edge = T.Tuple String Int

buildGrid :: Effect (Array Tile)
buildGrid = 
    readTextFile UTF8 "./src/Day12/input.txt"
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
     { c: (SCU.singleton heightChar) <> show xCoord <> show yCoord
     , x: xCoord
     , y: yCoord
     } 

buildGraph :: Array Tile -> G.Graph String Int
buildGraph = (buildAdjacencyList >>> G.fromAdjacencyList)

buildAdjacencyList :: Array Tile -> G.AdjacencyList String Int
--buildAdjacencyList ts = map (buildNode ts) ts # L.fromFoldable
buildAdjacencyList ts = A.foldl (buildNode ts) ts [] # L.fromFoldable

buildNode :: Array Tile -> Array Tile -> Tile -> Array Node
buildNode checked tiles tile = do
    let 
        n = A.find (\t -> t.x == tile.x && t.y == tile.y - 1) tiles
        e = A.find (\t -> t.x == tile.x + 1 && t.y == tile.y) tiles
        s = A.find (\t -> t.x == tile.x && t.y == tile.y + 1) tiles
        w = A.find (\t -> t.x == tile.x - 1 && t.y == tile.y) tiles

        neighbors = 
            A.catMaybes [n,e,s,w] 
                # A.filter (_.c >>> isValidNeighbor tile.c)
                # A.filter (_.c 
                --# spy ("validNeighbors:" <> show tile.c)

        node = T.Tuple (tile.c) (L.fromFoldable (neighbors # map tileToEdge))

    node
    
position = "abcdefghijklmnopqrstuvwxyz"
isValidNeighbor :: String -> String -> Boolean
isValidNeighbor a b = do
    let
        a' = case S.take 1 a of
                  "S" -> spy ("S:" <> a) $ "a"
                  l -> l

        
        b' = case S.take 1 b of
                  "E" -> spy ("E:" <> b) $ "z"
                  l -> l

        a'' = S.indexOf (S.Pattern (a')) position # M.fromMaybe (0)
        b'' = S.indexOf (S.Pattern (b')) position # M.fromMaybe (0)

        diff = b'' - a''

    --diff == 1 || diff == 0
    diff <= 1


tileToEdge :: Tile -> Edge
tileToEdge dst = do
    -- TODO(harry): Decide if we need to weight the edges?
    T.Tuple dst.c 1





    
-- NOT 309
-- NOT 310
