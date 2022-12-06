module Day6.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String.CodeUnits as SCU
import Data.Array as A

--

run :: Int -> Effect Unit
run markerLength = 
    readTextFile UTF8 "./src/Day6/input.txt"
    <#> SCU.toCharArray
    <#> A.dropEnd 1
    <#> findMarkerIndex markerLength 0
    >>= (show >>> log)

runA :: Effect Unit
runA = run 4

runB :: Effect Unit
runB = run 14

--

type MarkerLength = Int

findMarkerIndex :: MarkerLength -> Int -> Array Char -> Int
findMarkerIndex ml i buffer =
    case (A.length <<< A.nub <<< A.take ml) buffer == ml of 
         true -> i + ml
         false -> findMarkerIndex ml (i + 1) (A.drop 1 buffer)
    
