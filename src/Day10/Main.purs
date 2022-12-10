module Day10.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as S
import Data.Array as A
import Data.Maybe as M
import Data.Int as I
import Data.Ord as Ord
import Data.Foldable (sum)

--

runA :: Effect Unit
runA = 
    parseInstructions
    <#> applyInstructions
    <#> _.signalStrengths
    <#> sum
    >>= (show >>> log)

runB :: Effect Unit
runB = 
    parseInstructions
    <#> applyInstructions
    <#> _.visual
    <#> S.joinWith ""
    >>= log
--

applyInstructions :: Array Instruction -> State
applyInstructions =
    A.foldl applyInstruction { x: 1, cycles: 0, signalStrengths: [], visual: ["\n"] }

parseInstructions :: Effect (Array Instruction)
parseInstructions =
    readTextFile UTF8 "./src/Day10/input.txt"
    <#> S.split (S.Pattern "\n")
    <#> A.dropEnd 1
    <#> A.mapMaybe parseInstruction
    <#> A.concat

type State =
    { x :: Int
    , cycles :: Int
    , signalStrengths :: Array SignalStrength
    , visual :: Array String
    }

type Cycles = Int
type SignalStrength = Int

data Instruction
    = NoOp 
    | Add Int

instance showInstruction :: Show Instruction where
    show (NoOp) = "noop"
    show (Add n) = "add " <> show n

parseInstruction :: String -> M.Maybe (Array Instruction)
parseInstruction line =
    case S.split (S.Pattern " ") line of 
         ["noop"] -> M.Just $ [NoOp]
         ["addx", n] -> M.Just $ [NoOp, Add (n # I.fromString # M.fromMaybe 0)]
         _ -> M.Nothing

applyInstruction :: State -> Instruction -> State
applyInstruction st instruction = do
    let
        c = st.cycles + 1

        strengths = 
            case (20 + (c `mod` 40)) of
                 40 -> A.snoc st.signalStrengths (c * st.x)
                 _ -> st.signalStrengths

        pixel = 
            case (st.cycles `mod` 40) of 
                 p | Ord.abs (p - st.x) < 2 -> "#"
                   | otherwise -> "."

        updateX = 
            case instruction of 
                NoOp -> 0
                Add n -> n

    st { x = st.x + updateX, cycles = c, signalStrengths = strengths, visual = A.snoc st.visual pixel }
