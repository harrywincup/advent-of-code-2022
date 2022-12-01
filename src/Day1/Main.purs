module Day1.Main where

import Prelude

import Debug (spy, traceM)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as String
import Data.String.Common (null)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.Int as Int
import Data.Foldable (sum)

--

runA :: Effect Unit
runA = 
    buildInventories
        <#> Array.last
        <#> sum
        <#> show
        >>= log

runB :: Effect Unit
runB =
    buildInventories
        <#> Array.reverse 
        <#> Array.take 3 
        <#> sum
        <#> show
        >>= log

--

buildInventories :: Effect (Array Int)
buildInventories =
    readTextFile UTF8 "./src/Day1/input.txt"
        <#> String.split (String.Pattern "\n")
        <#> map Int.fromString
        <#> Array.groupBy isConsecutiveNumber
        <#> map toArray
        <#> Array.filter isElfInventory
        <#> map countCalories
        <#> Array.sort

isConsecutiveNumber :: Maybe Int -> Maybe Int -> Boolean
isConsecutiveNumber a b = isJust a && isJust b

isElfInventory :: Array (Maybe Int) -> Boolean
isElfInventory [Nothing] = false
isElfInventory _ = true

countCalories :: Array (Maybe Int) -> Int
countCalories = Array.foldl (\acc n -> acc + (fromMaybe 0 n)) 0








