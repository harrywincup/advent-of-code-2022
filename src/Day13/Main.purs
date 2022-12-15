module Day13.Main where

import Prelude

import Debug (spy)
import Effect (Effect)
import Effect.Console (log)
--import Control.Lazy (defer)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Char (toCharCode)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.List as L
import Data.Maybe as M
import Data.Int as I
import Data.Tuple as T
import Data.Ord as Ord
import Data.Foldable (sum, product)
import Data.Either as E
import Data.Traversable (traverse)
import Data.Graph as G
import Data.Enum (fromEnum)
import Parsing as P
import Parsing.String as PS
import Parsing.String.Basic as PSB
import Parsing.Combinators as PC
import Parsing.Combinators.Array as PCA

--

runA :: Effect Unit
runA = 
    readTextFile UTF8 "./src/Day13/test-input.txt"
    <#> S.split (S.Pattern "\n")
    <#> A.dropEnd 1
    <#> traverse parsePacket
    <#> E.fromRight []
    --<#> A.groupBy (\a b -> not S.null a && not S.null b)
    --<#> map NEA.toArray
    --<#> A.filter (not A.null)
    >>= (show >>> log)

--

--type PacketValue = Array Int
data PacketValue
    = Single Int
    | Nested (Array PacketValue)

instance showPacketValue :: Show PacketValue where
    show pvs = case pvs of 
        Single ns -> "Single " <> show ns
        _ -> show pvs 


parsePacket :: String -> E.Either P.ParseError (Array PacketValue)
parsePacket s = P.runParser s packetParser

packetParser :: P.Parser String (Array PacketValue)
packetParser = do
    _ <- PS.char '['
    ns <- PCA.many (intParser PC.<|> (packetParser <#> Nested))
    _ <- PS.char ']'

    pure ns

intParser :: P.Parser String PacketValue
intParser = do
   n <- PSB.intDecimal
   _ <- PC.optional (PS.char ',')
   pure $ Single n

       

--packetValueParser :: P.Parser String PacketValue
--packetValueParser = 
--    defer \_ -> PC.many (intParser PC.<|> PCA.many packetValueParser)
--
--intParser :: P.Parser String PacketValue
--intParser = do
--    n <- PSB.intDecimal
--    _ <- PC.optional (PS.char ',')
--
--    pure $ Single [n]


