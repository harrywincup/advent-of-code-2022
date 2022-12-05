module Day5.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Int as I
import Data.Array as A
import Data.Array.ST as AST
import Control.Monad.ST.Internal (ST)
import Data.Maybe (Maybe(..), fromMaybe)

--

runA :: Effect Unit
runA = 
    readTextFile UTF8 "./src/Day5/input.txt"
    <#> S.split (S.Pattern "\n")
    <#> A.dropEnd 1
    <#> modelData CM9001
    <#> runCrane CM9001
    >>= (show >>> log)

--

data CraneModel = CM9000 | CM9001

type Stack = Array Char

type Count = Int
type SourceID = Int
type DestinationID = Int
data MoveInstruction = MoveInstruction Count SourceID DestinationID

type State = 
    { stacks :: Array Stack
    , instructions :: Array MoveInstruction
    }

instance showMoveInstruction :: Show MoveInstruction where
    show (MoveInstruction count src dst) =
        "Move " <> (show count) <> " " <> (show src) <> " " <> (show dst)

modelData :: CraneModel -> Array String -> State
modelData craneModel lines = 
    let 
        parts = lines
            # A.span (not S.null) 

        emptyStacks = parts.init
            # A.takeEnd 1
            # map (S.split (S.Pattern " ") >>> A.filter (not S.null))
            # A.concat
            # A.length
            # (flip A.replicate) []

        stacks = parts.init
            # A.dropEnd 1
            # A.reverse
            # A.foldl parseStacksFromLine emptyStacks

        instructions = parts.rest
            # A.filter (not S.null)
            # A.concatMap (parseInstruction craneModel)
            # A.catMaybes

     in
    { stacks: stacks
    , instructions: instructions
    }

parseStacksFromLine :: Array Stack -> String -> Array Stack
parseStacksFromLine stacks line =
    stacks 
        # A.mapWithIndex (\i s -> A.snoc s $ fromMaybe '!' $ SCU.charAt ((i + 1) * 4 - 3) line)
        # map (A.filter (\c -> c /= ' '))

parseInstruction :: CraneModel -> String -> Array (Maybe MoveInstruction)
parseInstruction craneModel line = 
    let
        parts = line
            # S.split (S.Pattern " ")
            # A.mapMaybe I.fromString

        instruction = 
            case parts of 
                [c, s, d] -> 
                    case craneModel of 
                         CM9000 -> A.replicate c $ Just $ MoveInstruction 1 (s - 1) (d - 1)
                         CM9001 -> A.singleton $ Just $ MoveInstruction c (s - 1) (d - 1)

                _ -> A.fromFoldable Nothing

     in
     instruction

runCrane :: CraneModel -> State -> String
runCrane craneModel { stacks, instructions } =
    applyInstructions craneModel instructions stacks
    # A.mapMaybe (A.last)
    # SCU.fromCharArray

applyInstructions :: CraneModel -> Array MoveInstruction -> Array Stack -> Array Stack
applyInstructions craneModel instructions stacks =
    case instructions of 
         [] -> stacks 
         is -> applyInstructions craneModel (A.drop 1 is) (applyInstruction craneModel stacks (A.head is))

applyInstruction :: CraneModel -> Array Stack -> Maybe MoveInstruction -> Array Stack
applyInstruction craneModel stacks instruction = 
    case instruction of 
         Nothing -> stacks
         Just (MoveInstruction count src dst) -> AST.run do
            -- This is my first time ever using Array.ST
            -- It definitely feels like there's a better way than
            -- what i'm doing here. The nested Arrays feel awkward
            -- because the top level is the mutable array, but all
            -- the stacks inside are still regular immutable structures
            mutableStacks <- AST.thaw stacks

            -- get crate that is being moved from src
            srcStack <- AST.peek src mutableStacks <#> fromMaybe []
            dstStack <- AST.peek dst mutableStacks <#> fromMaybe []

            crates <- case craneModel of
                  CM9000 -> pure $ A.takeEnd 1 srcStack
                  CM9001 -> pure $ A.takeEnd count srcStack

            -- put the updated src stack back, minus the moved elements
            _ <- AST.modify src (A.dropEnd $ A.length crates) mutableStacks

            -- place crate into dst
            _ <- AST.modify dst (\_ -> dstStack <> crates) mutableStacks

            -- return modified array
            pure mutableStacks
            
            








