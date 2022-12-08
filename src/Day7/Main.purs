module Day7.Main where

import Prelude

import Debug (spy)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String as S
import Data.Array as A
import Data.List as L
import Data.Either as E
import Data.Maybe (Maybe(..))
import Data.Tree as T
import Data.Tree.Zipper as TZ
import Data.Tuple (Tuple(..), snd)
import Parsing as P
import Parsing.String as PS
import Parsing.String.Basic as PSB
import Parsing.Combinators as PC

--

runA :: Effect Unit
runA = 
    let 
        defaultTree = T.mkTree (DirInfo "/") (L.Nil)
        defaultLoc = TZ.fromTree defaultTree
    in
        readTextFile UTF8 "./src/Day7/test-input.txt"
        <#> S.split (S.Pattern "\n")
        <#> A.dropEnd 1
        <#> A.foldl buildFileSystem (Tuple defaultLoc defaultTree)
        <#> snd >>> T.showTree
        >>= (show >>> log)
--

type FileSystem = Tuple (TZ.Loc Entity) (T.Tree Entity)

buildFileSystem :: FileSystem -> String -> FileSystem
buildFileSystem (Tuple loc tree) line =
    case spy "Found" $ parseLogLine line of 
         E.Right entity -> 
             case entity of 
                (Cmd LS) -> Tuple loc tree
                (Cmd (CD "/")) -> Tuple (TZ.root loc) tree
                (Cmd (CD "..")) -> 
                    case TZ.up loc of
                         Nothing -> spy "Couldnt move up" $ Tuple loc tree
                         Just newLoc -> spy "Moving up: " $ Tuple newLoc tree

                (Cmd (CD path)) -> 
                    case TZ.findDown (DirInfo path) loc of 
                         Nothing -> spy ("Couldn't move into path:" <> path <> (show (TZ.value loc))) $ Tuple loc tree
                         Just newLoc -> Tuple newLoc tree

                (DirInfo dir) -> Tuple loc $ T.appendChild (T.mkTree (DirInfo dir) (L.Nil)) tree
                (FileInfo name size) -> Tuple loc $ T.appendChild (T.mkTree (FileInfo name size) (L.Nil)) tree

         E.Left _ -> Tuple loc tree

data Command
    = CD String
    | LS


derive instance eqCommand :: Eq Command
instance showCommand :: Show Command where
    show = case _ of 
            CD s -> "cd " <> s
            LS -> "ls"

data Entity 
    = Cmd Command 
    | DirInfo String
    | FileInfo String Int

derive instance eqEntity :: Eq Entity

instance showEntity :: Show Entity where
    show (Cmd cmd) = show cmd
    show (DirInfo dir) = "Dir: " <> dir
    show (FileInfo name size) = "File: " <> name <> " (" <> show size <> ")"

parseLogLine :: String -> E.Either P.ParseError Entity
parseLogLine line =
    P.runParser (spy "parsing" $ line) $ PC.choice [ commandParser, fileInfoParser, dirInfoParser ]

commandParser :: P.Parser String Entity
commandParser = do
    _   <- PS.char '$'
    _   <- PSB.space
    cmd <- PC.choice [cdCmdParser, lsCmdParser]

    pure (Cmd cmd)

cdCmdParser :: P.Parser String Command
cdCmdParser = do
    _       <- PS.string "cd"
    _       <- PSB.space
    path    <- PS.rest

    pure $ CD path

lsCmdParser :: P.Parser String Command
lsCmdParser = do
    _ <- PS.string "ls"

    pure $ LS

fileInfoParser :: P.Parser String Entity
fileInfoParser = do
    size    <- PSB.intDecimal
    _       <- PSB.space
    name    <- PS.rest

    pure $ FileInfo name size

dirInfoParser :: P.Parser String Entity
dirInfoParser = do
    _      <- PS.string "dir"
    _      <- PSB.space
    drive  <- PS.rest

    pure $ DirInfo drive



{-
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
-}


{-
    - / (dir)
      - a (dir)
        - e (dir)
          - i (file, size=584)
        - f (file, size=29116)
        - g (file, size=2557)
        - h.lst (file, size=62596)
      - b.txt (file, size=14848514)
      - c.dat (file, size=8504156)
      - d (dir)
        - j (file, size=4060174)
        - d.log (file, size=8033020)
        - d.ext (file, size=5626152)
        - k (file, size=7214296)
-}
