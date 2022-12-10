module Day9.Main where

import Prelude

import Debug (spy)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as S
import Data.Array as A
import Data.Maybe as M
import Data.Function as Fn
import Data.Int as I
import Data.Ord as Ord
import Data.Map as Map

--

runA :: Effect Unit
runA = 
    readTextFile UTF8 "./src/Day9/input.txt"
    <#> S.split (S.Pattern "\n")
    <#> A.dropEnd 1
    <#> A.foldl moveRope { hx: 0, hy: 0, tx: 0, ty: 0, visits: Map.singleton "0::0" 1 }
    <#> _.visits >>> Map.size
    >>= (show >>> log)

--

type State =
    { hx :: Int
    , hy :: Int
    , tx :: Int
    , ty :: Int
    , visits :: Map.Map String Int
    }

--type Visits = Array (Map.Map String Int)

type Delta = { dx :: Int, dy :: Int }

moveRope :: State -> String -> State
moveRope state m = do
    let 
        parts = S.split (S.Pattern " ") m
        direction = A.index parts 0 

        steps :: Int
        steps = M.fromMaybe 0 $ join $ I.fromString <$> A.index parts 1

        delta :: Delta
        delta = case direction of
                     M.Just "U" -> { dx: 0, dy: (-1) }
                     M.Just "D" -> { dx: 0, dy: 1 }
                     M.Just "L" -> { dx: (-1), dy: 0 }
                     M.Just "R" -> { dx: 1, dy: 0}
                     _          -> { dx: 0, dy: 0}

    -- TODO(harry): Find a way to map over slices of an array
    -- of knots so that we can reuse the exact same code for each pair
    -- and then take the tail at the end
    Fn.applyN (move delta) steps state

move :: Delta -> State -> State
move d st = do
   let 
       headX = st.hx + d.dx
       headY = st.hy + d.dy
       distX = headX - st.tx
       distY = headY - st.ty
       absX = Ord.abs distX
       absY = Ord.abs distY
       shouldMoveTail = absX > 1 || absY > 1
       tailX = if shouldMoveTail && distX /= 0 then st.tx + (Ord.signum distX) else st.tx
       tailY = if shouldMoveTail && distY /= 0 then st.ty + (Ord.signum distY) else st.ty
       visits = if shouldMoveTail then Map.insert (show tailX <> "::" <> show tailY) 1 st.visits else st.visits

   st { hx = headX, hy = headY, tx = tailX, ty = tailY, visits = visits }






