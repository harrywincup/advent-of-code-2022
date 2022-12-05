module Day4.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as S
import Data.Int as I
import Data.Array as A

--

runA :: Effect Unit
runA = run assignmentsContainNestedRange

runB :: Effect Unit
runB = run assignmentsContainOverlappingRanges

run :: (Array (Array Int) -> Boolean) -> Effect Unit
run filterFn =
    readTextFile UTF8 "./src/Day4/input.txt"   
    <#> S.split (S.Pattern "\n")
    <#> (map (splitPair >>> map parseRange))
    <#> A.filter filterFn
    <#> A.length
    >>= (show >>> log)
--

splitPair :: String -> Array String
splitPair = S.split (S.Pattern ",")

parseRange :: String -> Array Int
parseRange = S.split (S.Pattern "-") >>> A.mapMaybe I.fromString

assignmentsContainNestedRange :: Array (Array Int) -> Boolean
assignmentsContainNestedRange = 
    case _ of 
        [[a1,a2], [b1,b2]] -> a1 >= b1 && a2 <= b2 || b1 >= a1 && b2 <= a2
        _ -> false

assignmentsContainOverlappingRanges :: Array (Array Int) -> Boolean
assignmentsContainOverlappingRanges = 
    case _ of 
         [[a1,a2], [b1,b2]] -> a1 <= b2 && b1 <= a2
         _ -> false
    





