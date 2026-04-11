{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Fzfx.Core (
    -- * Domain Types
    SearchMode (..),
    GitStatus (..),
    FdType (..),
    defaultFdType,
    PreviewMode (..),
    PreviewLayout (..),
    OutMode (..),
    FzfItem (..),
    Subcmd (..),
    Config (..),

    -- * State Machine
    Event (..),
    ToggleName (..),
    FzfAction (..),
    transition,
    renderActions,

    -- * Subcmd Registry
    flg,
    parseSubcmd,

    -- * Config
    cfgPath,

    -- * Query & Line Parsing
    parseQuery,
    parseSFilter,
    parseFzfItem,
    tryRg,
    tryBookmark,
    stripAnsi,

    -- * FzfItem Accessors
    lineFile,
    lineRef,

    -- * Path Utilities
    makeRelPath,

    -- * List Utilities
    interleave,
    ordNub,

    -- * Fzf Action Rendering
    fzfWrap,

    -- * Display
    hdrText,
    gitStatusChar,
    showT,

    -- * Conversion
    decodeOut,
    t,
) where

import Control.Monad (guard)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.FilePath (isAbsolute, pathSeparator, splitDirectories, (</>))
import System.FilePath qualified as FP
import Text.Read (readMaybe)

-- ═══════════════════════════════════════════════════════════════════════
-- Domain Types
-- ═══════════════════════════════════════════════════════════════════════

data SearchMode
    = FileMode
    | RgLive !Text ![Text] -- pattern, extra args
    | RgLocked !Text !Text ![Text] -- pattern, filter, extra args
    | FzfRg !Text !Text ![Text] -- fzf filter, rg pattern, extra args
    | FzfRgPending !Text -- fzf filter (waiting for rg input after #)
    deriving (Eq, Show)

data GitStatus = Unstaged | Staged | Untracked | Clean
    deriving (Eq, Ord, Show, Read)

data FdType = FdFiles | FdDirs | FdMixed
    deriving (Eq, Read, Show)

defaultFdType :: FdType
defaultFdType = FdMixed

data PreviewMode = Content | Diff
    deriving (Eq, Read, Show)

data PreviewLayout = PreviewRight | PreviewBottom
    deriving (Eq, Read, Show)

data OutMode = OTmux | OStdout
    deriving (Eq, Read, Show)

data FzfItem
    = RgLine !Text !Int !Int -- file, line, col
    | BookmarkLine !Text !Int !Int -- file:line:col (no trailing text)
    | FdLine !GitStatus !Text
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
    | SSwap -- TODO: rename this
    | SFullPreview
    | SQueryPush
    | SQueryPop
    | SQueryApply
    | SQueryDelete
    | SQueryList
    | SSelSave
    | SSelRestore
    | SSmartEnter
    | SExtraArgs
    | SPreviewWidth
    | SZoxide
    | STokei
    | SHeightToggle
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
    SExtraArgs -> "--extra-args"
    SPreviewWidth -> "--preview-width"
    SZoxide -> "--zoxide"
    STokei -> "--tokei"
    SHeightToggle -> "--height-toggle"

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
    , cPreview :: !PreviewMode
    , cFileQuery :: !Text -- saved file mode query
    , cDirQuery :: !Text -- saved dir mode query
    , cQueryStack :: ![Text] -- query history stack
    , cPendingQuery :: !Text -- selected query from pop, applied by transform
    , cWasRg :: !Bool -- was last mode an rg mode?
    , cSavedFileSel :: ![Text] -- saved file mode selections
    , cSavedDirSel :: ![Text] -- saved dir mode selections
    , cGitSt :: !Bool -- git status filter (only dirty files)
    , cPreviewOn :: !Bool -- preview visible
    , cPreviewLayout :: !PreviewLayout -- preview position (right/bottom)
    , cHeight :: !Text -- fzf --height value (e.g. "40%", "~100%")
    , cHeightAuto :: !Bool -- True when height was resolved from "auto"
    , cMinHeight :: !Int -- fzf --min-height (0 = don't pass)
    , cPrompt :: !Text -- custom prompt override (empty = default)
    , cMixed :: !Bool -- mixed mode preference (files+dirs together)
    , cStripPrefix :: !Text -- prefix to strip from output paths (for completion)
    }
    deriving (Eq, Read, Show) -- Read/Show is fine: state dirs are ephemeral per-session, never shared between versions

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
            else
                let (pat, ex) = splitEx rgPat
                 in FzfRg filt pat ex
    | otherwise = FileMode
  where
    parseFzfRg s = case T.breakOn "#" s of
        (before, rest)
            | not (T.null before)
            , not (T.null rest) ->
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

parseFzfItem :: Text -> FzfItem
parseFzfItem raw = case tryRg stripped of
    Just (f, ln, col) -> RgLine f ln col
    Nothing -> case tryBookmark stripped of
        Just (f, ln, col) -> BookmarkLine f ln col
        Nothing -> case T.breakOn "\t" raw of
            (lbl, rest)
                | not (T.null rest) -> FdLine (toSt (T.strip lbl)) (T.drop 1 rest)
                | otherwise -> FdLine Clean (T.strip raw)
  where
    stripped = stripAnsi raw
    toSt "U" = Unstaged
    toSt "S" = Staged
    toSt "?" = Untracked
    toSt _ = Clean

-- | Parse rg output: file:line:col:text
tryRg :: Text -> Maybe (Text, Int, Int)
tryRg s = do
    let (file, rest1) = T.break (== ':') s
    guard (not (T.null file))
    r1 <- T.stripPrefix ":" rest1
    let (lnS, rest2) = T.break (== ':') r1
    ln <- readMaybe (T.unpack lnS)
    r2 <- T.stripPrefix ":" rest2
    let (colS, rest3) = T.break (== ':') r2
    col <- readMaybe (T.unpack colS)
    guard (not (T.null rest3))
    pure (file, ln, col)

-- | Parse bookmark: file:line:col or file:line (col defaults to 1)
tryBookmark :: Text -> Maybe (Text, Int, Int)
tryBookmark s = do
    let (file, rest1) = T.break (== ':') s
    guard (not (T.null file))
    r1 <- T.stripPrefix ":" rest1
    guard (not (T.null r1))
    let (lnS, rest2) = T.break (== ':') r1
    ln <- readMaybe (T.unpack lnS)
    if T.null rest2
        then pure (file, ln, 1)
        else do
            r2 <- T.stripPrefix ":" rest2
            col <- readMaybe (T.unpack r2)
            pure (file, ln, col)

stripAnsi :: Text -> Text
stripAnsi txt = case T.uncons txt of
    Nothing -> txt
    Just ('\ESC', rest) -> case T.uncons rest of
        Just ('[', rest') -> stripAnsi (T.drop 1 (T.dropWhile (\c -> c < '@' || c > '~') rest'))
        Just (_, rest') -> stripAnsi rest'
        Nothing -> txt
    Just (c, rest) -> T.cons c (stripAnsi rest)

lineFile :: FzfItem -> Text
lineFile (RgLine f _ _) = f
lineFile (BookmarkLine f _ _) = f
lineFile (FdLine _ p) = p

lineRef :: FzfItem -> Text
lineRef (RgLine f ln col) = f <> ":" <> showT ln <> ":" <> showT col
lineRef (BookmarkLine f ln col) = f <> ":" <> showT ln <> ":" <> showT col
lineRef (FdLine _ p) = p

-- ═══════════════════════════════════════════════════════════════════════
-- Path Utilities
-- ═══════════════════════════════════════════════════════════════════════

{- | Make a path relative from orig to cwd, using ../ up to 2 levels.
Falls back to absolute path beyond that.
-}
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
    joinPath ps = L.foldl1' (</>) ps

-- ═══════════════════════════════════════════════════════════════════════
-- List Utilities
-- ═══════════════════════════════════════════════════════════════════════

{- | Interleave sorted extras into main list, inserting each extra
just before the first main item that sorts after it.
-}
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

-- ═══════════════════════════════════════════════════════════════════════
-- Header Text
-- ═══════════════════════════════════════════════════════════════════════

hdrText :: Config -> Text
hdrText Config{..} =
    let on s = "\ESC[1;32m" <> s <> "\ESC[0m"
        off s = "\ESC[2m" <> s <> "\ESC[0m"
        dim s = "\ESC[2m" <> s <> "\ESC[0m"
        tog True = on
        tog False = off
        sep = " \x2502 "
        tilde p = maybe p ("~/" <>) $ T.stripPrefix "/home/" p >>= (T.stripPrefix "/" . T.dropWhile (/= '/'))
        navLine
            | cFd == FdDirs = "\n" <> dim ("cwd: " <> tilde cCwd)
            | cCwd == cOrig = ""
            | otherwise = "\n" <> dim ("cwd: " <> tilde cCwd <> "  (from " <> tilde cOrig <> ")")
        typeIndicator
            | cMixed =
                "C-t "
                    <> (if cFd == FdMixed then on "mixed" else off "mixed")
                    <> "/"
                    <> (if cFd == FdDirs then on "dirs" else off "dirs")
            | otherwise =
                "C-t "
                    <> (if cFd == FdFiles then on "files" else off "files")
                    <> "/"
                    <> (if cFd == FdDirs then on "dirs" else off "dirs")
     in T.intercalate
            sep
            [ typeIndicator
            , "M-m " <> tog cMixed "mixed"
            , "M-h " <> tog cHid "hid"
            , "M-i " <> tog cIgn "ign"
            , "C-p " <> (if cPreviewLayout == PreviewRight then off "→" else on "↓")
            , "M-g " <> tog cGitSt "git changed"
            , "C-M-g " <> (if cPreview == Diff then on "diff" else off "diff")
            , "C-f " <> (if cHeightAuto then dim "auto" else dim "full")
            , "M-u/s/?"
            , "M-a " <> tog cAt "@"
            ]
            <> navLine

-- ═══════════════════════════════════════════════════════════════════════
-- State Machine
-- ═══════════════════════════════════════════════════════════════════════

-- | Events that trigger state transitions
data Event
    = EvToggle !ToggleName !Text -- toggle name, current query
    | EvTransform !Text -- query changed (the query text)
    | EvSwap !Text -- swap query format
    | EvExtraArgs !Text -- insert " -- " for extra rg args
    | EvSmartEnter !Bool -- is alt-enter?
    | EvQueryPush !Text -- save query to stack
    | EvQueryDelete !Text -- remove query from stack
    deriving (Eq, Show)

data ToggleName
    = TgAtPrefix
    | TgDiff
    | TgHidden
    | TgNoIgnore
    | TgGitStatus
    | TgPreviewLayout
    | TgType -- auto-toggles between [files|mixed]/dirs
    | TgTypeD -- switch to dirs
    | TgTypeF -- switch to files
    | TgTypeMixed -- switch to mixed (files+dirs)
    | TgMixed -- toggle mixed mode on/off (M-m)
    deriving (Eq, Show)

-- | FZF actions to emit (composed with +)
data FzfAction
    = ChangePrompt !Text
    | ChangeQuery !Text
    | ChangeFooter !Text -- pre-rendered footer text
    | ReloadSync !Text -- command to run
    | EnableSearch
    | DisableSearch
    | RefreshPreview
    | JumpFirst
    | Accept
    | ChangePreviewWindow !Text -- change-preview-window(...)
    | Become !Text -- become: command
    | Execute !Text -- execute(): command
    deriving (Eq, Show)

-- | Pure state transition: config + event → (new config, fzf actions)
transition :: Config -> Event -> (Config, [FzfAction])
-- Toggle: @ prefix
transition cfg (EvToggle TgAtPrefix _) =
    let cfg' = cfg{cAt = not (cAt cfg)}
     in (cfg', [ChangeFooter (hdrText cfg')])
-- Toggle: diff/content preview
transition cfg (EvToggle TgDiff _) =
    let cfg' = cfg{cPreview = if cPreview cfg == Content then Diff else Content}
     in (cfg', [RefreshPreview, ChangeFooter (hdrText cfg')])
-- Toggle: hidden files
transition cfg (EvToggle TgHidden _) =
    let cfg' = cfg{cHid = not (cHid cfg)}
     in (cfg', [reloadAction cfg', ChangeFooter (hdrText cfg')])
-- Toggle: no-ignore
transition cfg (EvToggle TgNoIgnore _) =
    let cfg' = cfg{cIgn = not (cIgn cfg)}
     in (cfg', [reloadAction cfg', ChangeFooter (hdrText cfg')])
-- Toggle: preview layout (right/bottom) — reload triggers result event for sizing
transition cfg (EvToggle TgPreviewLayout _) =
    let lay = if cPreviewLayout cfg == PreviewRight then PreviewBottom else PreviewRight
        cfg' = cfg{cPreviewLayout = lay}
        dir = case lay of PreviewRight -> "right"; PreviewBottom -> "bottom"
     in (cfg', [ChangePreviewWindow (dir <> ":50%"), reloadAction cfg', ChangeFooter (hdrText cfg')])
-- Toggle: git status filter (only dirty files) — also toggles diff preview
transition cfg (EvToggle TgGitStatus _) =
    let on = not (cGitSt cfg)
        cfg' = cfg{cGitSt = on, cPreview = if on then Diff else Content}
     in (cfg', [reloadAction cfg', RefreshPreview, ChangeFooter (hdrText cfg')])
-- Toggle: type auto (dispatch to D or F/Mixed)
transition cfg (EvToggle TgType q) =
    let target = case cFd cfg of
            FdDirs -> if cMixed cfg then TgTypeMixed else TgTypeF
            _ -> TgTypeD
     in transition cfg (EvToggle target q)
-- Toggle: switch to dirs mode
transition cfg (EvToggle TgTypeD curQ)
    | cFd cfg == FdDirs = (cfg, []) -- already in dirs mode
    | otherwise =
        let restoreQ = cDirQuery cfg
            cfg' = cfg{cFd = FdDirs, cFileQuery = curQ}
            resetPos = [JumpFirst | T.null restoreQ]
         in ( cfg'
            , [ reloadWithQuery cfg' restoreQ
              , ChangePrompt "dirs> "
              , ChangeQuery restoreQ
              ]
                <> resetPos
                <> [ChangeFooter (hdrText cfg')]
            )
-- Toggle: switch to files mode
transition cfg (EvToggle TgTypeF curQ)
    | cFd cfg == FdFiles = (cfg, []) -- already in files mode
    | otherwise =
        let restoreQ = cFileQuery cfg
            cfg' = cfg{cFd = FdFiles, cMixed = False, cDirQuery = curQ}
         in ( cfg'
            ,
                [ reloadWithQuery cfg' restoreQ
                , ChangePrompt "files> "
                , ChangeQuery restoreQ
                , ChangeFooter (hdrText cfg')
                ]
            )
-- Toggle: switch to mixed mode (files+dirs)
transition cfg (EvToggle TgTypeMixed curQ)
    | cFd cfg == FdMixed = (cfg, []) -- already in mixed mode
    | otherwise =
        let restoreQ = cFileQuery cfg
            cfg' = cfg{cFd = FdMixed, cMixed = True, cDirQuery = curQ}
         in ( cfg'
            ,
                [ reloadWithQuery cfg' restoreQ
                , ChangePrompt "mixed> "
                , ChangeQuery restoreQ
                , ChangeFooter (hdrText cfg')
                ]
            )
-- Toggle: mixed mode on/off (M-m)
transition cfg (EvToggle TgMixed curQ) =
    case cFd cfg of
        FdFiles -> transition cfg{cMixed = True} (EvToggle TgTypeMixed curQ)
        FdMixed -> transition cfg{cMixed = False} (EvToggle TgTypeF curQ)
        FdDirs ->
            -- Just toggle the preference for when we switch back
            let cfg' = cfg{cMixed = not (cMixed cfg)}
             in (cfg', [ChangeFooter (hdrText cfg')])
-- Transform: query changed — update prompt, search mode, reload
transition cfg (EvTransform q) =
    let mode = parseQuery q
        isRg = case mode of FileMode -> False; _ -> True
        wasRg = cWasRg cfg
        -- Auto-switch from dirs to files when entering rg mode
        autoSwitchFd = isRg && cFd cfg == FdDirs
        nFd = if isRg then FdFiles else cFd cfg
        cfg' =
            cfg
                { cWasRg = isRg
                , cFd = if autoSwitchFd then FdFiles else cFd cfg
                }
        hdrUpd = [ChangeFooter (hdrText cfg') | autoSwitchFd]
        promptName = case nFd of FdDirs -> "dirs"; FdMixed -> "mixed"; FdFiles -> "files"
        act = case mode of
            FileMode
                | wasRg -> [ChangePrompt (promptName <> "> "), EnableSearch, reloadAction cfg']
                | otherwise -> [ChangePrompt (promptName <> "> "), EnableSearch]
            RgLive{} -> [ChangePrompt "rg> ", DisableSearch, reloadAction cfg']
            RgLocked{} -> [ChangePrompt "filter> ", DisableSearch, reloadAction cfg']
            FzfRg{} -> [ChangePrompt "fzf#rg> ", DisableSearch, reloadAction cfg']
            FzfRgPending{} -> [ChangePrompt "fzf#> ", DisableSearch, reloadAction cfg']
     in (cfg', act <> hdrUpd)
-- ExtraArgs: insert " -- " at the right position for rg extra args
transition cfg (EvExtraArgs q) =
    let result = case parseQuery q of
            RgLive _ _ -> Just (q <> " -- -")
            RgLocked{} ->
                -- Insert before the second # : #pat → #pat -- -#filter
                case T.stripPrefix "#" q of
                    Just body -> case T.breakOn "#" body of
                        (before, rest)
                            | not (T.null rest) ->
                                Just ("#" <> before <> " -- -" <> rest)
                        _ -> Just (q <> " -- -")
                    Nothing -> Nothing
            FzfRg{} -> Just (q <> " -- -")
            FzfRgPending _ -> Nothing -- no rg pattern yet
            FileMode -> Nothing -- not in rg mode
     in case result of
            Just q' -> (cfg, [ChangeQuery q'])
            Nothing -> (cfg, [])
-- Swap: switch between #rg#filter and filter#rg query formats
transition cfg (EvSwap q) =
    let swapped = case parseQuery q of
            RgLive pat _ -> pat <> "#"
            RgLocked pat f _ -> f <> "#" <> pat
            FzfRg f pat _ -> "#" <> pat <> "#" <> f
            FzfRgPending f -> "#" <> f
            FileMode -> q <> "#"
     in (cfg, [ChangeQuery swapped])
-- Smart enter: dirs mode navigates, files mode accepts/edits
transition cfg (EvSmartEnter isAlt)
    | cFd cfg == FdDirs =
        (cfg, [Become (cSelf cfg <> " " <> flg SNavigate <> " into {} {q}")])
    | isAlt =
        (cfg, [Execute (cSelf cfg <> " " <> flg SEdit <> " {}")])
    | otherwise =
        (cfg, [Accept])
-- Query push: add to stack (dedup against top)
transition cfg (EvQueryPush q) =
    let stack = cQueryStack cfg
        stack' =
            if T.null q || (case stack of x : _ -> x == q; [] -> False)
                then stack
                else q : stack
     in (cfg{cQueryStack = stack'}, [])
-- Query delete: remove from stack
transition cfg (EvQueryDelete q) =
    let stack' = filter (/= q) (cQueryStack cfg)
     in (cfg{cQueryStack = stack'}, [])

-- | Helper: build a reload-sync action using cSelf
reloadAction :: Config -> FzfAction
reloadAction cfg = ReloadSync (cSelf cfg <> " " <> flg SReload <> " {q}")

-- | Helper: build a reload-sync with a specific query (not {q})
reloadWithQuery :: Config -> Text -> FzfAction
reloadWithQuery cfg q = ReloadSync (cSelf cfg <> " " <> flg SReload <> " " <> q)

{- | Wrap an fzf action name and argument, choosing a delimiter that doesn't
conflict with the argument text.  fzf supports () [] <> ~~ as delimiters.
-}
fzfWrap :: Text -> Text -> Text
fzfWrap action arg
    | not (T.isInfixOf ")" arg) = action <> "(" <> arg <> ")"
    | not (T.isInfixOf "]" arg) = action <> "[" <> arg <> "]"
    | not (T.isInfixOf ">" arg) = action <> "<" <> arg <> ">"
    | not (T.isInfixOf "~" arg) = action <> "~" <> arg <> "~"
    | otherwise = action <> "(" <> arg <> ")" -- fallback

-- | Render a list of FzfActions to the fzf protocol string
renderActions :: [FzfAction] -> Text
renderActions = T.intercalate "+" . map render1
  where
    render1 (ChangePrompt p) = fzfWrap "change-prompt" p
    render1 (ChangeQuery q) = fzfWrap "change-query" q
    render1 (ChangeFooter h) = fzfWrap "change-border-label" h
    render1 (ReloadSync cmd) = fzfWrap "reload-sync" cmd
    render1 EnableSearch = "enable-search"
    render1 DisableSearch = "disable-search"
    render1 RefreshPreview = "refresh-preview"
    render1 (ChangePreviewWindow w) = fzfWrap "change-preview-window" w
    render1 JumpFirst = "first"
    render1 Accept = "accept"
    render1 (Become cmd) = "become:" <> cmd
    render1 (Execute cmd) = fzfWrap "execute" cmd
