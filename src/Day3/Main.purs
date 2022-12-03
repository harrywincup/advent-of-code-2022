module Day3.Main where

import Prelude

import Debug (spy)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as String
import Data.String.CodeUnits (toCharArray, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (sum)
import Data.Tuple (Tuple(..))
import Data.Array ((:), filter, intersect, nub, head, foldl, drop, take, uncons)

--

runA :: Effect Unit
runA = 
    loadRucksackContents
    <#> buildRucksacks
    <#> map scoreRucksack
    <#> sum
    >>= (show >>> log)

runB :: Effect Unit
runB = 
    loadRucksackContents
    <#> assignToGroups
    <#> map findBadgePriorityForGroup
    <#> sum
    >>= (show >>> log)

--

type Compartment = Array Char

type Rucksack = 
    { a :: Compartment
    , b :: Compartment
    }

loadRucksackContents :: Effect (Array String)
loadRucksackContents =
    readTextFile UTF8 "./src/Day3/input.txt"
    <#> String.split (String.Pattern "\n")

buildRucksacks :: Array String -> Array Rucksack
buildRucksacks = filter (not String.null) >>> map buildRucksack

buildRucksack :: String -> Rucksack
buildRucksack s = do
    let l = String.length s
    let parts = String.splitAt (l / 2) s

    { a: toCharArray parts.before
    , b: toCharArray parts.after
    }

scoreRucksack :: Rucksack -> Int
scoreRucksack { a, b } =
    intersect a b 
        # nub
        # map findPriority
        # sum


charPriority :: String
charPriority = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

findPriority :: Char -> Int
findPriority c =
    charPriority
        # String.indexOf (String.Pattern (singleton c))
        # map ((+) 1)
        # fromMaybe 0


assignToGroups :: Array String -> Array (Array String)
assignToGroups = case _ of 
    [] -> []
    xs -> take 3 xs : assignToGroups (drop 3 xs)

findBadgePriorityForGroup :: Array String -> Int
findBadgePriorityForGroup group = 
    case uncons group of 
         Nothing -> 0
         Just { head: x, tail: xs } ->
            xs
                # map toCharArray
                # foldl intersect (toCharArray x)
                # nub
                # map findPriority
                # sum


