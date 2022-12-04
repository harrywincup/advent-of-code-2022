module Day4.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as String
import Data.Int as Int
import Data.Array (mapMaybe, filter, length)

--

runA :: Effect Unit
runA = run assignmentsContainNestedRange

runB :: Effect Unit
runB = run assignmentsContainOverlappingRanges

run :: (Array (Array Int) -> Boolean) -> Effect Unit
run filterFn =
    readTextFile UTF8 "./src/Day4/input.txt"   
    <#> String.split (String.Pattern "\n")
    <#> (map (splitPair >>> map parseRange))
    <#> filter filterFn
    <#> length
    >>= (show >>> log)
--

splitPair :: String -> Array String
splitPair = String.split (String.Pattern ",")

parseRange :: String -> Array Int
parseRange = String.split (String.Pattern "-") >>> mapMaybe Int.fromString

assignmentsContainNestedRange :: Array (Array Int) -> Boolean
assignmentsContainNestedRange as = 
    case as of 
         [[a1,a2], [b1,b2]] -> a1 >= b1 && a2 <= b2 || b1 >= a1 && b2 <= a2
         _ -> false

assignmentsContainOverlappingRanges :: Array (Array Int) -> Boolean
assignmentsContainOverlappingRanges as = 
    case as of 
         [[a1,a2], [b1,b2]] -> a1 <= b2 && b1 <= a2
         _ -> false
    





