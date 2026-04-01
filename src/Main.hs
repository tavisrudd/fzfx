{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- fzf-insert-files: FZF file picker with self-referencing subcommand dispatch
-- Build: ghc -O2 -Wall src/Main.hs (with typed-process, text, bytestring)

module Main (main) where

import Control.Exception (IOException, bracket, catch)
import Control.Monad (unless, void)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.List (partition, sort)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    getCurrentDirectory,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    setCurrentDirectory,
 )
import System.Environment (getArgs, getExecutablePath, lookupEnv, setEnv)
import System.Exit (exitWith)
import System.FilePath (isAbsolute, isRelative, splitDirectories, takeDirectory, takeExtension, (</>))
import System.FilePath qualified as FP
import System.IO (hFlush, readFile', stdout)
import System.Posix.IO (dupTo, stdError, stdInput, stdOutput)
import System.Posix.Process (executeFile, getProcessID)
import System.Process.Typed hiding (setEnv)

-- ═══════════════════════════════════════════════════════════════════════
-- Domain Types
-- ═══════════════════════════════════════════════════════════════════════

data SearchMode
    = FileMode
    | RgLive Text [Text] -- pattern, extra args
    | RgLocked Text Text [Text] -- pattern, filter, extra args
    | FzfRg Text Text [Text] -- fzf filter, rg pattern, extra args
    | FzfRgPending Text -- fzf filter (waiting for rg input after #)

data GitStatus = Unstaged | Staged | Untracked | Clean
    deriving (Eq, Ord)

data FdType = FdFiles | FdDirs
    deriving (Eq, Read, Show)

data PrevMode = Content | Diff
    deriving (Eq, Read, Show)

data OutMode = OTmux | OStdout
    deriving (Eq, Read, Show)

data LineInfo
    = RgLine Text Int -- file, line number
    | FdLine GitStatus Text

-- ═══════════════════════════════════════════════════════════════════════
-- Subcmd Registry (compile-time exhaustive flag → handler mapping)
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
    deriving (Eq, Enum, Bounded)

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

parseSubcmd :: String -> Maybe Subcmd
parseSubcmd s = lookup s [(T.unpack (flg c), c) | c <- [minBound .. maxBound]]

dispatch :: Subcmd -> [String] -> IO ()
dispatch sub rest = case sub of
    SReload -> cmdReload (toArg rest)
    SPreview -> cmdPreview (toArg rest)
    STransform -> cmdTransform (toArg rest)
    SToggle -> cmdToggle (toArg rest)
    SNavigate -> case rest of
        (a : l : q : _) -> cmdNavigate (T.pack a) (T.pack l) (T.pack q)
        _ -> cmdNavigate "" "" ""
    SEdit -> cmdEdit (toArg rest)
    SMagit -> cmdMagit (toArg rest)
    SForgit -> cmdForgit (toArg rest)
    SCopy -> cmdCopy (toArg rest)
    SDebug -> cmdDebug
    SSwap -> cmdSwap (toArg rest)
  where
    toArg = T.pack . unwords

-- ═══════════════════════════════════════════════════════════════════════
-- Config (Read/Show-serialized state)
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
    }
    deriving (Read, Show)

cfgPath :: Text -> FilePath
cfgPath d = T.unpack d </> "cfg"

saveConfig :: Config -> IO ()
saveConfig c = do
    createDirectoryIfMissing True (T.unpack (cDir c))
    writeFile (cfgPath (cDir c)) (show c)

loadConfig :: IO Config
loadConfig = do
    d <- getStateDir
    s <- readFile' (cfgPath d)
    pure (read s)

-- | Load config and cd to its working directory before running the handler
withCfg :: (Config -> IO a) -> IO a
withCfg f = do
    c <- loadConfig
    setCurrentDirectory (t (cCwd c))
    f c

modConfig :: (Config -> Config) -> IO Config
modConfig f = do
    c <- loadConfig
    let c' = f c
    writeFile (cfgPath (cDir c')) (show c')
    pure c'

getStateDir :: IO Text
getStateDir =
    lookupEnv "_FZFX_STATE_DIR" >>= \case
        Just d | not (null d) -> pure (T.pack d)
        _ -> error "_FZFX_STATE_DIR not set"

-- ═══════════════════════════════════════════════════════════════════════
-- Utilities
-- ═══════════════════════════════════════════════════════════════════════

-- | Decode process stdout as Text
decodeOut :: LBS.ByteString -> Text
decodeOut = TE.decodeUtf8Lenient . LBS.toStrict

-- | Convert Text to FilePath (single point of conversion)
t :: Text -> FilePath
t = T.unpack

-- | Run a process, capture stdout as Text
readProc :: Text -> [Text] -> IO Text
readProc cmd args = decodeOut . fst <$> readProcess_ (proc (t cmd) (map t args))

-- | Run a process, return Nothing on failure (does not use cwd)
readProcMaybe :: Text -> [Text] -> IO (Maybe Text)
readProcMaybe cmd args = do
    (ec, out, _err) <- readProcess $ proc (t cmd) (map t args)
    pure $ case ec of
        ExitSuccess -> Just (T.strip $ decodeOut out)
        _ -> Nothing

-- | Run a process with inherited stdio
exec :: Text -> [Text] -> IO ()
exec cmd args = runProcess_ $ proc (t cmd) (map t args)

-- | Pipe one process's stdout into another's stdin
piped :: (Text, [Text]) -> (Text, [Text]) -> IO ()
piped (c1, a1) (c2, a2) = do
    let p1 = setStdout createPipe $ setStderr nullStream $ proc (t c1) (map t a1)
    withProcessWait_ p1 $ \proc1 ->
        runProcess_ $ setStdin (useHandleOpen (getStdout proc1)) $ proc (t c2) (map t a2)

showT :: (Show a) => a -> Text
showT = T.pack . show

envOr :: String -> Text -> IO Text
envOr k d = maybe d T.pack <$> lookupEnv k

envOrM :: String -> IO Text -> IO Text
envOrM k m =
    lookupEnv k >>= \case
        Just v | not (null v) -> pure (T.pack v)
        _ -> m

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
-- Subcommand Handlers
-- ═══════════════════════════════════════════════════════════════════════

cmdReload :: Text -> IO ()
cmdReload q = withCfg $ \Config{..} ->
    case parseQuery q of
        FileMode -> reloadFiles cGit cFd cHid cIgn q
        RgLive p ex -> reloadRgLive p ex
        FzfRg filt rgPat ex -> reloadFzfRg filt rgPat ex
        FzfRgPending filt -> reloadFzfRgPending filt
        RgLocked p f ex -> reloadRgLocked p f ex

reloadFiles :: Text -> FdType -> Bool -> Bool -> Text -> IO ()
reloadFiles gitRoot fdType fdHid fdIgn query = do
    let (sf, _) = parseSFilter query
        ty = case fdType of FdFiles -> "f"; FdDirs -> "d"
        fa =
            [ "--type"
            , ty
            , "--exclude"
            , ".git"
            , "--exclude"
            , "node_modules"
            , "--strip-cwd-prefix"
            ]
                <> ["--hidden" | fdHid]
                <> ["--no-ignore-vcs" | fdIgn]
                <> ["-L" | fdIgn]
    out <- readProc "fd" fa
    let files = filter (not . T.null) (T.lines out)
    labeled <-
        if T.null gitRoot
            then pure [(Clean, f) | f <- files]
            else do
                pfx <- T.strip <$> readProc "git" ["rev-parse", "--show-prefix"]
                u <- Set.fromList . filter (not . T.null) . T.lines <$> readProc "git" ["diff", "--name-only"]
                s <- Set.fromList . filter (not . T.null) . T.lines <$> readProc "git" ["diff", "--cached", "--name-only"]
                tr <- Set.fromList . filter (not . T.null) . T.lines <$> readProc "git" ["ls-files", "--others", "--exclude-standard"]
                let classify f = let rp = pfx <> f in
                      case () of
                        _ | Set.member rp u  -> Unstaged
                          | Set.member rp s  -> Staged
                          | Set.member rp tr -> Untracked
                          | otherwise        -> Clean
                pure [(classify f, f) | f <- files]
    let ok = filter (\(st, _) -> maybe True (== st) sf) labeled
        (top, bot) = partition (\(_, f) -> "src/" `T.isPrefixOf` f || "test/" `T.isPrefixOf` f) ok
    mapM_ (\(st, f) -> TIO.putStrLn (gitStatusChar st <> "\t" <> f)) (top <> bot)
    hFlush stdout

reloadRgLive :: Text -> [Text] -> IO ()
reloadRgLive pat ex = unless (T.null pat) $ do
    let args =
            ["--column", "--line-number", "--no-heading", "--color=always", "--smart-case"]
                <> ex
                <> ["--", pat]
    runProcess_ $ setStdout inherit $ setStderr nullStream $ proc (t "rg") (map t args)

reloadRgLocked :: Text -> Text -> [Text] -> IO ()
reloadRgLocked pat filt ex = unless (T.null pat) $ do
    let rgArgs =
            ["--files-with-matches", "--no-heading", "--color=never", "--smart-case", "--sort=path"]
                <> ex
                <> ["--", pat]
    out <- catch (readProc "rg" rgArgs) (\(_ :: IOException) -> pure "")
    let files = filter (not . T.null) (T.lines out)
    filtered <- if T.null filt then pure files
                else do
                    let input = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines files
                    fout <- catch
                        (decodeOut . fst <$> readProcess_ (setStdin (byteStringInput input) $ proc "fzf" ["--filter", t filt]))
                        (\(_ :: IOException) -> pure "")
                    pure $ filter (not . T.null) (T.lines fout)
    mapM_ (\f -> TIO.putStrLn (" \t" <> f)) filtered
    hFlush stdout

reloadFzfRg :: Text -> Text -> [Text] -> IO ()
reloadFzfRg filt rgPat ex = unless (T.null rgPat) $ do
    -- Get all files, filter with fzf, then rg within matches
    fdOut <- readProc "fd" ["--type", "f", "--exclude", ".git", "--exclude", "node_modules", "--strip-cwd-prefix"]
    let allFiles = filter (not . T.null) (T.lines fdOut)
    targets <- if T.null filt then pure allFiles
               else do
                   let input = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines allFiles
                   fout <- catch
                       (decodeOut . fst <$> readProcess_ (setStdin (byteStringInput input) $ proc "fzf" ["--filter", t filt]))
                       (\(_ :: IOException) -> pure "")
                   pure $ filter (not . T.null) (T.lines fout)
    unless (null targets) $ do
        let rgArgs = ["--column", "--line-number", "--no-heading", "--color=always", "--smart-case"]
                <> ex <> ["--", rgPat] <> targets
        runProcess_ $ setStdout inherit $ setStderr nullStream $ proc "rg" (map t rgArgs)

reloadFzfRgPending :: Text -> IO ()
reloadFzfRgPending filt = do
    fdOut <- readProc "fd" ["--type", "f", "--exclude", ".git", "--exclude", "node_modules", "--strip-cwd-prefix"]
    let allFiles = filter (not . T.null) (T.lines fdOut)
    filtered <- if T.null filt then pure allFiles
                else do
                    let input = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines allFiles
                    fout <- catch
                        (decodeOut . fst <$> readProcess_ (setStdin (byteStringInput input) $ proc "fzf" ["--filter", t filt]))
                        (\(_ :: IOException) -> pure "")
                    pure $ filter (not . T.null) (T.lines fout)
    mapM_ (\f -> TIO.putStrLn (" \t" <> f)) filtered
    hFlush stdout

cmdPreview :: Text -> IO ()
cmdPreview line = withCfg $ \Config{..} -> do
    cols <- maybe 80 readInt <$> lookupEnv "FZF_PREVIEW_COLUMNS"
    rows <- maybe 40 readInt <$> lookupEnv "FZF_PREVIEW_LINES"
    case parseLine line of
        RgLine file ln ->
            let start = max 1 (ln - rows `div` 2)
            in exec "bat" ["--color=always", "--style=numbers", "--highlight-line", showT ln, "--line-range", showT start <> ":", "--", file]
        FdLine st path
            | cPrev == Diff, st /= Clean -> diffPrev st path cols
            | otherwise -> contentPrev path
  where
    readInt s = case reads s of [(n, _)] -> n; _ -> 80 :: Int

diffPrev :: GitStatus -> Text -> Int -> IO ()
diffPrev st path cols = do
    let da = case st of
            Unstaged -> ["diff", "--", path]
            Staged -> ["diff", "--cached", "--", path]
            Untracked -> ["diff", "--no-index", "--", "/dev/null", path]
            Clean -> []
    if null da
        then contentPrev path
        else
            if cols >= 80
                then piped ("git", da) ("delta", ["--width=" <> showT cols])
                else exec "git" (da <> ["--color=always"])

contentPrev :: Text -> IO ()
contentPrev path = do
    isDir <- doesDirectoryExist (t path)
    if isDir
        then exec "eza" ["--icons", "--git-ignore", "--git", "--tree", "-L", "3", "--color=always", path]
        else
            if takeExtension (t path) == ".ipynb"
                then exec "nbpreview" [path]
                else exec "bat" [path, "--style=plain", "--color=always", "--line-range", "0:100"]

cmdEdit :: Text -> IO ()
cmdEdit line = withCfg $ \_ -> do
    reopenTty
    case parseLine line of
        RgLine f ln -> executeFile "tr-edit" True [t ("+" <> showT ln), t f] Nothing
        FdLine _ p -> executeFile "tr-edit" True [t p] Nothing

-- | Restore stdin/stdout to the terminal for become: handlers.
-- fzfx pipes fzf's stdin (reload data) and stdout (selection capture),
-- but stderr is inherited as the real pty throughout fzfx → fzf → become.
-- Dup stderr over the piped fds so child processes get a proper terminal.
reopenTty :: IO ()
reopenTty = do
    _ <- dupTo stdError stdInput
    void $ dupTo stdError stdOutput

cmdDebug :: IO ()
cmdDebug = withCfg $ \cfg -> do
    reopenTty
    let args = fzfArgs cfg
        self = cSelf cfg
        short = "fzfx"
        content = T.unlines $ toggleSummary cfg <> ["", "# fzfArgs  (fzfx=" <> self <> ")", ""] <> map (formatArg self short) args <> ["", "# config", ""] <> prettyConfig cfg
    runProcess_ $
        setStdin (byteStringInput (LBS.fromStrict (TE.encodeUtf8 content))) $
            proc "less" ["-R"]
  where
    formatArg self short a = case T.breakOn "=" (T.replace self short a) of
        (key, val)
            | "--bind" `T.isPrefixOf` key ->
                let (bkey, bval) = T.breakOn ":" (T.drop 1 val)
                in "  " <> dim key <> "=" <> bold bkey <> prettifyBash bval
            | not (T.null val) -> "  " <> dim key <> "=" <> T.drop 1 val
            | otherwise -> "  " <> a
    bold s = "\ESC[1;33m" <> s <> "\ESC[0m"
    dim s = "\ESC[2m" <> s <> "\ESC[0m"
    toggleSummary Config{..} =
        let on  label = "\ESC[1;32m" <> label <> "\ESC[0m"
            off label = "\ESC[2m" <> label <> "\ESC[0m"
            tog True  = on
            tog False = off
        in  [ "# toggles"
            , ""
            , "  " <> bold "M-a"   <> "  @ prefix    " <> if cAt then on "ON" else off "off"
            , "  " <> bold "C-d"   <> "  type         " <> (if cFd == FdDirs then on "dirs" else off "dirs")
                                                   <> " / " <> (if cFd == FdFiles then on "files" else off "files")
            , "  " <> bold "C-h"   <> "  hidden       " <> if cHid then on "ON" else off "off"
            , "  " <> bold "M-i"   <> "  no-ignore    " <> if cIgn then on "ON" else off "off"
            , "  " <> bold "M-g"   <> "  preview      " <> (if cPrev == Diff then on "diff" else off "diff")
                                                   <> " / " <> (if cPrev == Content then on "content" else off "content")
            , "  " <> bold "M-a"   <> "  output       " <> (case cOut of OTmux -> on "tmux"; OStdout -> on "stdout")
            ]
    prettifyBash s = foldl (\acc (pat, rep) -> T.replace pat rep acc) s
        [ ("; then ", ";\n      then ")
        , ("; elif ", ";\n      elif ")
        , ("; else ", ";\n      else ")
        , ("; fi", ";\n      fi")
        , (";;", ";;\n      ")
        , ("; esac", ";\n      esac")
        ]
    prettyConfig Config{..} =
        let fields =
                [ ("cDir",   "state_dir",     cDir)
                , ("cGit",   "git_root",      cGit)
                , ("cOrig",  "orig_cwd",      cOrig)
                , ("cCwd",   "cwd",           cCwd)
                , ("cPane",  "pane",          cPane)
                , ("cSelf",  "self",          cSelf)
                , ("cQuery", "query",         cQuery)
                , ("cOut",   "output_mode",   showT cOut)
                , ("cAt",    "at_prefix",     showT cAt)
                , ("cFd",    "fd_type",       showT cFd)
                , ("cHid",   "hidden",        showT cHid)
                , ("cIgn",   "no_ignore",     showT cIgn)
                , ("cPrev",  "preview_mode",  showT cPrev)
                ]
            pad n s = s <> T.replicate (max 0 (n - T.length s)) " "
        in map (\(f, desc, v) -> pad 7 f <> pad 14 desc <> " = " <> v) fields

-- | Swap between #rg#filter and filter#rg query formats
cmdSwap :: Text -> IO ()
cmdSwap q = do
    let swapped = case parseQuery q of
            RgLive pat _     -> pat <> "#"
            RgLocked pat f _ -> f <> "#" <> pat
            FzfRg f pat _    -> "#" <> pat <> "#" <> f
            FzfRgPending f   -> "#" <> f
            FileMode         -> q <> "#"
    TIO.putStr ("change-query(" <> swapped <> ")") >> hFlush stdout

cmdMagit :: Text -> IO ()
cmdMagit line = withCfg $ \_ ->
    exec "magit-file-status" [lineFile (parseLine line)]

cmdForgit :: Text -> IO ()
cmdForgit line = withCfg $ \_ ->
    exec "git-forgit" ["log", "--", lineFile (parseLine line)]

cmdCopy :: Text -> IO ()
cmdCopy line = do
    let path = T.stripEnd $ lineFile (parseLine line)
    runProcess_ $
        setStdin (byteStringInput (LBS.fromStrict (TE.encodeUtf8 path))) $
            proc "tmux" ["load-buffer", "-w", "-"]

cmdTransform :: Text -> IO ()
cmdTransform q = withCfg $ \Config{..} -> do
    let rl = cSelf <> " " <> flg SReload <> " {q}"
        act = case parseQuery q of
            FileMode -> "change-prompt(files> )+enable-search+reload(" <> rl <> ")"
            RgLive{} -> "change-prompt(rg> )+disable-search+reload(" <> rl <> ")"
            RgLocked{} -> "change-prompt(filter> )+disable-search+reload(" <> rl <> ")"
            FzfRg{} -> "change-prompt(fzf#rg> )+disable-search+reload(" <> rl <> ")"
            FzfRgPending{} -> "change-prompt(fzf#> )+disable-search+reload(" <> rl <> ")"
    TIO.putStr act >> hFlush stdout

hdrText :: Config -> Text
hdrText Config{..} =
    let on  s = "\ESC[1;32m" <> s <> "\ESC[0m"
        off s = "\ESC[2m" <> s <> "\ESC[0m"
        dim s = "\ESC[2m" <> s <> "\ESC[0m"
        tog True  = on
        tog False = off
        sep = " \x2502 "
        tilde p = maybe p ("~/" <>) $ T.stripPrefix "/home/" p >>= (T.stripPrefix "/" . T.dropWhile (/= '/'))
        navLine
            | cCwd == cOrig = ""
            | otherwise = "\n" <> dim ("cwd: " <> tilde cCwd <> "  (from " <> tilde cOrig <> ")")
    in  T.intercalate sep
        [ "C-" <> (if cFd == FdFiles then on "f" else off "f")
            <> "/" <> (if cFd == FdDirs then on "d" else off "d")
        , "C-h " <> tog cHid "hid"
        , "M-i " <> tog cIgn "ign"
        , "C-/ prev"
        , "M-u/s/?"
        , "M-g " <> (if cPrev == Diff then on "diff" else off "diff")
        , "M-a " <> tog cAt "@"
        ] <> navLine

cmdToggle :: Text -> IO ()
cmdToggle name = do
    c <- loadConfig
    let rl = "reload(" <> cSelf c <> " " <> flg SReload <> " {q})"
        hdr c' = "+change-header(" <> hdrText c' <> ")"
    case name of
        "at_prefix" -> do
            let c' = c{cAt = not (cAt c)}
            void $ modConfig $ \x -> x{cAt = cAt c'}
            TIO.putStr $ "change-header(" <> hdrText c' <> ")"
        "diff" -> do
            let c' = c{cPrev = if cPrev c == Content then Diff else Content}
            void $ modConfig $ \x -> x{cPrev = cPrev c'}
            TIO.putStr $ "refresh-preview" <> hdr c'
        "hidden" -> do
            let c' = c{cHid = not (cHid c)}
            void $ modConfig $ \x -> x{cHid = cHid c'}
            TIO.putStr $ rl <> hdr c'
        "no_ignore" -> do
            let c' = c{cIgn = not (cIgn c)}
            void $ modConfig $ \x -> x{cIgn = cIgn c'}
            TIO.putStr $ rl <> hdr c'
        "type_d" -> do
            let c' = c{cFd = FdDirs}
            void $ modConfig $ \x -> x{cFd = FdDirs}
            TIO.putStr $ rl <> hdr c'
        "type_f" -> do
            let c' = c{cFd = FdFiles}
            void $ modConfig $ \x -> x{cFd = FdFiles}
            TIO.putStr $ rl <> hdr c'
        _ -> pure ()
    hFlush stdout

cmdNavigate :: Text -> Text -> Text -> IO ()
cmdNavigate navAction sel query = do
    Config{..} <- loadConfig
    let sp = T.dropWhileEnd (== '/') $ lineFile (parseLine sel)
    nCwd <- case navAction of
        "into" -> do
            d <- doesDirectoryExist (t cCwd </> t sp)
            pure $
                if d
                    then cCwd <> "/" <> sp
                    else T.pack (takeDirectory (t cCwd </> t sp))
        "up" -> pure $ T.pack (takeDirectory (t cCwd))
        "root" -> pure $ if T.null cGit then cCwd else cGit
        "toggle_root" ->
            pure $
                if not (T.null cGit) && cCwd == cGit
                    then cOrig
                    else if T.null cGit then cCwd else cGit
        _ -> pure cCwd
    nGit <- detectGit nCwd
    void $ modConfig $ \x -> x{cCwd = nCwd, cGit = nGit}
    setEnv "_FZFX_CWD" (t nCwd)
    setEnv "_FZFX_QUERY" (t query)
    setEnv "_FZFX_AT_PREFIX" (if cAt then "1" else "0")
    let nFd = if navAction == "into" then FdFiles else cFd
    setEnv "_FZFX_FDTYPE" (case nFd of FdFiles -> "f"; FdDirs -> "d")
    setEnv "_FZFX_HIDDEN" (if cHid then "--hidden" else "")
    setEnv "_FZFX_STATE_DIR" ""
    mainLaunch []

-- | Make a path relative from orig to cwd, using ../ up to 2 levels.
-- Falls back to absolute path beyond that.
makeRelPath :: Text -> Text -> Text -> Text
makeRelPath orig cwd path
    | isAbsolute (t path) = path
    | orig == cwd = path
    | otherwise =
        let absPath = t cwd </> t path
            origParts = splitDirectories (t orig)
            absParts = splitDirectories absPath
            common = length $ takeWhile id $ zipWith (==) origParts absParts
            ups = length origParts - common
            rest = drop common absParts
        in if ups > 2
           then T.pack absPath
           else T.pack $ joinPath (replicate ups ".." <> rest)
  where
    joinPath [] = "."
    joinPath ps = foldl1 (</>) ps

detectGit :: Text -> IO Text
detectGit dir = fromMaybe "" <$> readProcMaybe "git" ["-C", dir, "rev-parse", "--show-toplevel"]

-- ═══════════════════════════════════════════════════════════════════════
-- FZF Argument Building
-- ═══════════════════════════════════════════════════════════════════════

bind :: Text -> Text -> Text
bind key act = "--bind=" <> key <> ":" <> act

xf :: Config -> Text -> Subcmd -> Text -> Text
xf Config{..} key sub rest =
    bind key $ "transform:" <> cSelf <> " " <> flg sub <> " " <> rest

xe :: Config -> Text -> Subcmd -> Text -> Text -> Text
xe Config{..} key sub rest sfx =
    bind key $ "execute(" <> cSelf <> " " <> flg sub <> " " <> rest <> ")" <> sfx

bc :: Config -> Text -> Subcmd -> Text -> Text
bc Config{..} key sub rest =
    bind key $ "become:" <> cSelf <> " " <> flg sub <> " " <> rest

-- | Status toggle: prepend/remove/replace status prefix in query
statusToggle :: Text -> Text
statusToggle pfx =
    "transform:q={q}; "
        <> "if [ \"${q#^"
        <> pfx
        <> " }\" != \"$q\" ]; then echo \"change-query:${q#^"
        <> pfx
        <> " }\"; "
        <> "elif echo \"$q\" | grep -q '^\\^[US?] '; then echo \"change-query:^"
        <> pfx
        <> " ${q#^? }\"; "
        <> "else echo \"change-query:^"
        <> pfx
        <> " $q\"; fi"

fzfArgs :: Config -> [Text]
fzfArgs cfg@Config{..} = baseOpts <> selfBindings <> staticBindings
  where
    baseOpts =
        [ "--ansi"
        , "--reverse"
        , "--tiebreak=index"
        , "--multi"
        , "--delimiter=\t"
        , "--with-nth=1,2"
        , "--header=" <> hdrText cfg
        , "--prompt=files> "
        , "--query=" <> cQuery
        , "--preview=" <> cSelf <> " " <> flg SPreview <> " {}"
        , "--preview-window=right:50%"
        ]
    selfBindings =
        [ xf cfg "change" STransform "{q}"
        , xf cfg "alt-a" SToggle "at_prefix"
        , bind "alt-p" "execute(line={}; if [[ \"$line\" == *:*:*:* ]]; then file=\"${line%%:*}\"; else file=$(echo \"$line\" | cut -f2); [ -z \"$file\" ] && file=\"$line\"; fi; fzf-preview \"$file\" 2>&1 | LESS='-Rc~' less)"
        , xf cfg "alt-g" SToggle "diff"
        , bind "alt-u" (statusToggle "U")
        , bind "alt-s" (statusToggle "S")
        , bind "alt-?" (statusToggle "?")
        , xf cfg "ctrl-h" SToggle "hidden"
        , xf cfg "alt-i" SToggle "no_ignore"
        , xf cfg "ctrl-d" SToggle "type_d"
        , xf cfg "ctrl-f" SToggle "type_f"
        , bc cfg "ctrl-o" SNavigate "into {} {q}"
        , bc cfg "ctrl-l" SNavigate "up {} {q}"
        , bc cfg "ctrl-r" SNavigate "root {} {q}"
        , bc cfg "alt-." SNavigate "toggle_root {} {q}"
        , bc cfg "alt-enter" SEdit "{}"
        , xe cfg "alt-," SMagit "{}" "+abort"
        , xe cfg "alt-c" SCopy "{}" "+abort"
        , xe cfg "ctrl-alt-l" SForgit "{}" ""
        , xe cfg "ctrl-alt-d" SDebug "" ""
        , xf cfg "alt-r" SSwap "{q}"
        ]
    staticBindings =
        [ bind "tab" "toggle+down"
        , bind "shift-tab" "toggle+down+end-of-line+unix-line-discard"
        , bind "alt-/" "toggle+up"
        , bind "ctrl-k" "kill-line"
        , bind "alt-k" "clear-query"
        , bind "alt--" "transform:q={q}; case $q in \\#*) printf change-query:{q}\\ --\\ -;; esac"
        , bind "alt-3" "transform:q={q}; case $q in \\#*) ;; *) printf change-query\\(\\#{q}\\)+beginning-of-line+forward-char;; esac"
        , bind "alt-b" "backward-word"
        , bind "alt-f" "forward-word"
        , bind "ctrl-/" "toggle-preview"
        , bind "alt-;" "change-preview-window(up,99%,border-bottom|right,50%,border-left)"
        , bind "alt-{" "preview-half-page-up"
        , bind "alt-}" "preview-half-page-down"
        , bind "alt-space" "preview-page-down"
        , bind "ctrl-space" "preview-page-up"
        , bind "ctrl-alt-g" "preview-top"
        , bind "ctrl-alt-G" "preview-bottom"
        , bind "ctrl-g" "abort"
        ]

-- ═══════════════════════════════════════════════════════════════════════
-- Output
-- ═══════════════════════════════════════════════════════════════════════

outputResults :: Config -> [Text] -> IO ()
outputResults Config{..} sel = do
    let parsed = map parseLine sel
        resolve li = case li of
            RgLine f ln -> makeRelPath cOrig cCwd f <> ":" <> showT ln
            FdLine _ p -> makeRelPath cOrig cCwd p
        files = dedup $ sort $ map resolve parsed
    case cOut of
        OStdout -> mapM_ TIO.putStrLn files
        OTmux -> do
            let pfx = if cAt then map ("@" <>) files else files
                txt = T.unwords pfx <> " "
            tmuxSend ["-H", "1b", "5b", "32", "30", "30", "7e"]
            tmuxSend ["-l", txt]
            tmuxSend ["-H", "1b", "5b", "32", "30", "31", "7e"]
  where
    tmuxSend extra = runProcess_ $ proc "tmux" (map t $ ["send-keys", "-t", cPane] <> extra)
    dedup [] = []
    dedup (x : xs) = x : dedup (dropWhile (== x) xs)

-- ═══════════════════════════════════════════════════════════════════════
-- Main / Launch
-- ═══════════════════════════════════════════════════════════════════════

main :: IO ()
main =
    getArgs >>= \case
        (cmd : rest) | Just sub <- parseSubcmd cmd -> dispatch sub rest
        ["-h"] -> printHelp
        ["--help"] -> printHelp
        other -> mainLaunch (map T.pack other)

printHelp :: IO ()
printHelp = TIO.putStr $ T.unlines
    [ "fzfx - FZF file picker with ripgrep integration"
    , ""
    , "Usage: fzfx [query]"
    , ""
    , "Query syntax:"
    , "  <text>          file search (fd)"
    , "  #<pattern>      live ripgrep search"
    , "  #<pat>#<filter> ripgrep then filter results"
    , "  \\#<text>        literal # in file search"
    , ""
    , "Keybindings:"
    , "  enter           select file(s) and output/insert"
    , "  alt-enter       open in editor (tr-edit)"
    , "  tab / alt-/     toggle selection down/up"
    , "  shift-tab       toggle + clear line"
    , "  alt-3           switch to ripgrep mode"
    , "  alt-a           toggle @ prefix on output"
    , "  alt-p           full-screen preview in less"
    , "  alt-g           toggle diff preview"
    , "  alt-c           copy path to tmux buffer"
    , "  alt-,           magit file status"
    , "  ctrl-alt-l      forgit log"
    , "  ctrl-o          navigate into directory"
    , "  ctrl-l          navigate up"
    , "  ctrl-r          navigate to git root"
    , "  alt-.           toggle root/original dir"
    , "  ctrl-h / alt-h  show/hide hidden files"
    , "  alt-i           toggle no-ignore"
    , "  ctrl-d / ctrl-f switch to dirs/files"
    , "  alt-u/s/?       filter by git status"
    , "  ctrl-/          toggle preview"
    , "  alt-;           cycle preview layout"
    , "  ctrl-g          abort"
    , ""
    , "Environment:"
    , "  _FZFX_CWD         starting directory"
    , "  _FZFX_OUTPUT_MODE stdout or tmux (default: tmux if in tmux)"
    , "  _FZFX_FDTYPE      fd type filter (f/d, default: f)"
    , "  _FZFX_HIDDEN      set to --hidden to show hidden files"
    , "  _FZFX_PANE        tmux target pane"
    , "  _FZFX_ORIG_CWD    original cwd (for toggle_root)"
    ]

mainLaunch :: [Text] -> IO ()
mainLaunch rest = do
    self <- T.pack <$> getExecutablePath
    cwd <- envOrM "_FZFX_CWD" (T.pack <$> getCurrentDirectory)
    orig <- envOr "_FZFX_ORIG_CWD" cwd
    omEnv <- envOr "_FZFX_OUTPUT_MODE" ""
    inTmux <- maybe False (not . null) <$> lookupEnv "TMUX"
    pane <-
        if not inTmux then pure ""
        else lookupEnv "_FZFX_PANE" >>= \case
            Just p | not (null p) -> pure (T.pack p)
            _ ->
                lookupEnv "TMUX_TARGET_PANE" >>= \case
                    Just p | not (null p) -> pure (T.pack p)
                    _ -> fromMaybe "" <$> readProcMaybe "tmux" ["display-message", "-p", "#{pane_id}"]
    let om = case omEnv of
                "stdout" -> OStdout
                "tmux"   -> OTmux
                _        -> if T.null pane then OStdout else OTmux
    at <- (== "1") <$> envOr "_FZFX_AT_PREFIX" "0"
    fd <- (\s -> if s == "d" then FdDirs else FdFiles) <$> envOr "_FZFX_FDTYPE" "f"
    hid <- T.isInfixOf "--hidden" <$> envOr "_FZFX_HIDDEN" ""
    q <-
        lookupEnv "_FZFX_QUERY" >>= \case
            Just v | not (null v) -> pure (T.pack v)
            _ -> pure (T.unwords rest)
    git <- detectGit cwd

    at' <-
        if not at && om == OTmux && not (T.null pane)
            then catch (detectAi pane) (\(_ :: IOException) -> pure False)
            else pure at

    tmp <- getTemporaryDirectory
    pid <- getProcessID
    let sd = T.pack $ tmp </> "fzf-insert-files-" <> show pid
        cfg = Config sd git orig cwd pane self q om at' fd hid False Content

    setCurrentDirectory (t cwd)
    setEnv "_FZFX_ORIG_CWD" (t orig)

    bracket
        (saveConfig cfg >> pure sd)
        (\d -> catch (removeDirectoryRecursive (t d)) (\(_ :: IOException) -> pure ()))
        $ \_ -> do
            setEnv "_FZFX_STATE_DIR" (t sd)
            -- Pipe reload output into fzf's stdin (fzf needs real tty on stderr)
            let reloadProc = setStdout createPipe
                           $ proc (t self) [t (flg SReload), t q]
                fzfCmd = map t (fzfArgs cfg)
            withProcessWait_ reloadProc $ \reloadP -> do
                let fzfProc = setStdin (useHandleOpen (getStdout reloadP))
                            $ setStdout createPipe
                            $ proc (t "fzf") fzfCmd
                withProcessWait fzfProc $ \p -> do
                    out <- LBS.hGetContents (getStdout p)
                    ec <- waitExitCode p
                    case ec of
                        ExitSuccess -> do
                            let selected = filter (not . T.null) (T.lines (decodeOut out))
                            unless (null selected) $ do
                                c <- loadConfig
                                outputResults c selected
                        _ -> exitWith ec

detectAi :: Text -> IO Bool
detectAi pane = do
    tty <- readProcMaybe "tmux" ["display-message", "-t", pane, "-p", "#{pane_tty}"]
    case tty of
        Nothing -> pure False
        Just tty' -> do
            (ec, out, _) <- readProcess $ proc "ps" ["-t", t tty', "-o", "comm="]
            pure $ case ec of
                ExitSuccess ->
                    any
                        (\p -> any (`T.isInfixOf` p) ["claude", "codex", "gemini"])
                        (filter (not . T.null) (T.lines (decodeOut out)))
                _ -> False
