{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Fzfx.Core
    ( -- * Domain Types
      SearchMode (..)
    , GitStatus (..)
    , FdType (..)
    , PrevMode (..)
    , OutMode (..)
    , LineInfo (..)
    , Subcmd (..)
    , Config (..)

      -- * Subcmd Registry
    , flg
    , parseSubcmd

      -- * Config
    , cfgPath

      -- * Query & Line Parsing
    , parseQuery
    , parseSFilter
    , parseLine
    , tryRg
    , stripAnsi

      -- * LineInfo Accessors
    , lineFile
    , lineRef

      -- * Path Utilities
    , makeRelPath

      -- * List Utilities
    , interleave
    , ordNub

      -- * Display
    , gitStatusChar
    , showT

      -- * Conversion
    , decodeOut
    , t
    ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.FilePath (isAbsolute, pathSeparator, splitDirectories, (</>))
import System.FilePath qualified as FP

-- ═══════════════════════════════════════════════════════════════════════
-- Domain Types
-- ═══════════════════════════════════════════════════════════════════════

data SearchMode
    = FileMode
    | RgLive Text [Text] -- pattern, extra args
    | RgLocked Text Text [Text] -- pattern, filter, extra args
    | FzfRg Text Text [Text] -- fzf filter, rg pattern, extra args
    | FzfRgPending Text -- fzf filter (waiting for rg input after #)
    deriving (Eq, Show)

data GitStatus = Unstaged | Staged | Untracked | Clean
    deriving (Eq, Ord, Show, Read)

data FdType = FdFiles | FdDirs
    deriving (Eq, Read, Show)

data PrevMode = Content | Diff
    deriving (Eq, Read, Show)

data OutMode = OTmux | OStdout
    deriving (Eq, Read, Show)

data LineInfo
    = RgLine Text Int -- file, line number
    | FdLine GitStatus Text
    deriving (Eq, Show)

-- ═══════════════════════════════════════════════════════════════════════
-- Subcmd Registry
-- ═══════════════════════════════════════════════════════════════════════

data Subcmd
    = SReload
    | SPreview
    | SEdit
    | STransform
    | SToggle
    | SNavigate
    | SMagit
    | SForgit
    | SCopy
    | SDebug
    | SSwap
    | SFullPreview
    | SQueryPush
    | SQueryPop
    | SQueryApply
    | SQueryDelete
    | SQueryList
    | SSelSave
    | SSelRestore
    | SSmartEnter
    deriving (Eq, Enum, Bounded, Show)

flg :: Subcmd -> Text
flg = \case
    SReload -> "--reload"
    SPreview -> "--preview"
    SEdit -> "--edit"
    STransform -> "--transform"
    SToggle -> "--toggle"
    SNavigate -> "--navigate"
    SMagit -> "--magit"
    SForgit -> "--forgit-log"
    SCopy -> "--copy"
    SDebug -> "--debug"
    SSwap -> "--swap"
    SFullPreview -> "--full-preview"
    SQueryPush -> "--query-push"
    SQueryPop -> "--query-pop"
    SQueryApply -> "--query-apply"
    SQueryDelete -> "--query-delete"
    SQueryList -> "--query-list"
    SSelSave -> "--sel-save"
    SSelRestore -> "--sel-restore"
    SSmartEnter -> "--smart-enter"

parseSubcmd :: String -> Maybe Subcmd
parseSubcmd s = lookup s [(T.unpack (flg c), c) | c <- [minBound .. maxBound]]

-- ═══════════════════════════════════════════════════════════════════════
-- Config
-- ═══════════════════════════════════════════════════════════════════════

data Config = Config
    { cDir :: !Text -- state directory
    , cGit :: !Text -- git root (empty if not in repo)
    , cOrig :: !Text -- original starting directory
    , cCwd :: !Text -- current working directory
    , cPane :: !Text -- tmux target pane
    , cSelf :: !Text -- path to self executable
    , cQuery :: !Text -- initial query
    , cOut :: !OutMode -- output mode
    , cAt :: !Bool -- @ prefix
    , cFd :: !FdType
    , cHid :: !Bool -- hidden files
    , cIgn :: !Bool -- no-ignore
    , cPrev :: !PrevMode
    , cFileQuery :: !Text -- saved file mode query
    , cDirQuery :: !Text -- saved dir mode query
    , cQueryStack :: ![Text] -- query history stack
    , cPendingQuery :: !Text -- selected query from pop, applied by transform
    , cWasRg :: !Bool -- was last mode an rg mode?
    , cSavedFileSel :: ![Text] -- saved file mode selections
    , cSavedDirSel :: ![Text] -- saved dir mode selections
    }
    deriving (Eq, Read, Show)

cfgPath :: Text -> FilePath
cfgPath d = T.unpack d </> "cfg"

-- ═══════════════════════════════════════════════════════════════════════
-- Conversion & Display Utilities
-- ═══════════════════════════════════════════════════════════════════════

-- | Decode process stdout as Text
decodeOut :: LBS.ByteString -> Text
decodeOut = TE.decodeUtf8Lenient . LBS.toStrict

-- | Convert Text to FilePath (single point of conversion)
t :: Text -> FilePath
t = T.unpack

showT :: (Show a) => a -> Text
showT = T.pack . show

gitStatusChar :: GitStatus -> Text
gitStatusChar = \case
    Unstaged -> "U"
    Staged -> "S"
    Untracked -> "?"
    Clean -> " "

-- ═══════════════════════════════════════════════════════════════════════
-- Query & Line Parsing
-- ═══════════════════════════════════════════════════════════════════════

parseQuery :: Text -> SearchMode
parseQuery q
    | "\\#" `T.isPrefixOf` q = FileMode
    | Just body <- T.stripPrefix "#" q = case T.breakOn "#" body of
        (p, rest)
            | not (T.null rest) ->
                let (pat, ex) = splitEx p
                 in RgLocked pat (T.drop 1 rest) ex
            | otherwise ->
                let (pat, ex) = splitEx p
                 in RgLive pat ex
    | Just (filt, rgPat) <- parseFzfRg q =
        if T.null (T.stripStart rgPat)
        then FzfRgPending filt
        else let (pat, ex) = splitEx rgPat
              in FzfRg filt pat ex
    | otherwise = FileMode
  where
    parseFzfRg s = case T.breakOn "#" s of
        (before, rest)
            | not (T.null before), not (T.null rest) ->
                Just (T.strip before, T.drop 1 rest)
            | otherwise -> Nothing
    splitEx s = case T.breakOn " -- " s of
        (p', rest')
            | T.null rest' -> (T.strip p', [])
            | otherwise -> (T.strip p', T.words (T.drop 4 rest'))

parseSFilter :: Text -> (Maybe GitStatus, Text)
parseSFilter q
    | Just r <- T.stripPrefix "^U " q = (Just Unstaged, r)
    | Just r <- T.stripPrefix "^S " q = (Just Staged, r)
    | Just r <- T.stripPrefix "^? " q = (Just Untracked, r)
    | otherwise = (Nothing, q)

parseLine :: Text -> LineInfo
parseLine raw = case tryRg (stripAnsi raw) of
    Just (f, ln) -> RgLine f ln
    Nothing -> case T.breakOn "\t" raw of
        (lbl, rest)
            | not (T.null rest) -> FdLine (toSt (T.strip lbl)) (T.drop 1 rest)
            | otherwise -> FdLine Clean (T.strip raw)
  where
    toSt "U" = Unstaged
    toSt "S" = Staged
    toSt "?" = Untracked
    toSt _ = Clean

-- | Parse rg output: file:line:col:text
tryRg :: Text -> Maybe (Text, Int)
tryRg s = case T.break (== ':') s of
    (file, rest1) | not (T.null file), Just r1 <- T.stripPrefix ":" rest1 ->
        case T.break (== ':') r1 of
            (lnS, rest2) | isAllDigit lnS, Just r2 <- T.stripPrefix ":" rest2 ->
                case T.break (== ':') r2 of
                    (colS, rest3) | isAllDigit colS, not (T.null rest3) ->
                        Just (file, read (T.unpack lnS))
                    _ -> Nothing
            _ -> Nothing
    _ -> Nothing
  where
    isAllDigit s' = not (T.null s') && T.all isDigit s'

stripAnsi :: Text -> Text
stripAnsi txt
    | T.null txt = txt
    | T.head txt == '\ESC' = case T.uncons (T.tail txt) of
        Just ('[', rest) -> stripAnsi (T.drop 1 (T.dropWhile (\c -> c < '@' || c > '~') rest))
        Just (_, rest) -> stripAnsi rest
        Nothing -> txt
    | otherwise = T.cons (T.head txt) (stripAnsi (T.tail txt))

lineFile :: LineInfo -> Text
lineFile (RgLine f _) = f
lineFile (FdLine _ p) = p

lineRef :: LineInfo -> Text
lineRef (RgLine f ln) = f <> ":" <> showT ln
lineRef (FdLine _ p) = p

-- ═══════════════════════════════════════════════════════════════════════
-- Path Utilities
-- ═══════════════════════════════════════════════════════════════════════

-- | Make a path relative from orig to cwd, using ../ up to 2 levels.
-- Falls back to absolute path beyond that.
makeRelPath :: Text -> Text -> Text -> Text
makeRelPath orig cwd path
    | isAbsolute (t path) = path
    | normOrig == normCwd = path
    | otherwise =
        let absPath = FP.normalise $ t cwd </> t path
            origParts = splitDirectories normOrig
            absParts = splitDirectories absPath
            common = length $ takeWhile id $ zipWith (==) origParts absParts
            ups = length origParts - common
            rest = drop common absParts
        in if ups > 2
           then T.pack absPath
           else T.pack $ joinPath (replicate ups ".." <> rest)
  where
    dropTrailingSep = reverse . dropWhile (== pathSeparator) . reverse
    normOrig = dropTrailingSep $ FP.normalise (t orig)
    normCwd = dropTrailingSep $ FP.normalise (t cwd)
    joinPath [] = "."
    joinPath ps = foldl1 (</>) ps

-- ═══════════════════════════════════════════════════════════════════════
-- List Utilities
-- ═══════════════════════════════════════════════════════════════════════

-- | Interleave sorted extras into main list, inserting each extra
-- just before the first main item that sorts after it.
interleave :: [Text] -> [Text] -> [Text]
interleave main' [] = main'
interleave [] extras = extras
interleave (m : ms) es@(e : es')
    | e <= m = e : interleave (m : ms) es'
    | otherwise = m : interleave ms es

ordNub :: [Text] -> [Text]
ordNub = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | Set.member x seen = go seen xs
        | otherwise = x : go (Set.insert x seen) xs
