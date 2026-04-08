{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception (IOException, bracket, catch)
import Control.Monad (unless, void, when)
import Data.ByteString.Lazy qualified as LBS
import Data.List (partition, sort)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Options.Applicative
import Options.Applicative.Help.Pretty (pretty)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getSymbolicLinkTarget,
    getTemporaryDirectory,
    pathIsSymbolicLink,
    removeDirectoryRecursive,
    setCurrentDirectory,
 )
import System.Environment (getArgs, getExecutablePath, lookupEnv, setEnv)

import System.FilePath (isAbsolute, pathSeparator, splitDirectories, takeDirectory, takeExtension, (</>))
import System.FilePath qualified as FP
import System.IO (hFlush, readFile', stdout)
import System.Posix.IO (dupTo, stdError, stdInput, stdOutput)
import System.Posix.Process (executeFile, getProcessID)
import System.Process.Typed hiding (setEnv)

import Fzfx.Core

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
    SFullPreview -> cmdFullPreview (toArg rest)
    SQueryPush -> cmdQueryPush (toArg rest)
    SQueryPop -> cmdQueryPop
    SQueryApply -> cmdQueryApply
    SQueryDelete -> cmdQueryDelete (toArg rest)
    SQueryList -> cmdQueryList
    SSelSave -> cmdSelSave (toArg rest)
    SSelRestore -> cmdSelRestore
    SSmartEnter -> cmdSmartEnter (toArg rest)
    SExtraArgs -> cmdExtraArgs (toArg rest)
    SPreviewWidth -> cmdPreviewWidth
    SZoxide -> cmdZoxide (toArg rest)
    STokei -> cmdTokei (toArg rest)
  where
    toArg = T.pack . unwords

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

envOr :: String -> Text -> IO Text
envOr k d = maybe d T.pack <$> lookupEnv k

envOrM :: String -> IO Text -> IO Text
envOrM k m =
    lookupEnv k >>= \case
        Just v | not (null v) -> pure (T.pack v)
        _ -> m

-- ═══════════════════════════════════════════════════════════════════════
-- Subcommand Handlers
-- ═══════════════════════════════════════════════════════════════════════

cmdReload :: Text -> IO ()
cmdReload q = withCfg $ \cfg ->
    case parseQuery q of
        FileMode -> reloadFiles cfg q
        RgLive p ex -> reloadRgLive p ex
        FzfRg filt rgPat ex -> reloadFzfRg filt rgPat ex
        FzfRgPending filt -> reloadFzfRgPending filt
        RgLocked p f ex -> reloadRgLocked p f ex

reloadFiles :: Config -> Text -> IO ()
reloadFiles Config{..} query = do
    let (sf, _) = parseSFilter query
        ty = case cFd of FdFiles -> "f"; FdDirs -> "d"
        fa =
            [ "--type"
            , ty
            , "--exclude"
            , ".git"
            , "--exclude"
            , "node_modules"
            , "--strip-cwd-prefix"
            ]
                <> ["--hidden" | cHid]
                <> ["-L"]
                <> ["--no-ignore-vcs" | cIgn]
    out <- readProc "fd" fa
    extraFiles <- loadFzfxInclude ty
    let mainFiles = sort $ filter (not . T.null) (T.lines out)
        mainSet = Set.fromList mainFiles
        newExtras = sort $ filter (\f -> not (Set.member f mainSet)) extraFiles
        files = interleave mainFiles newExtras
    labeled <-
        if T.null cGit
            then pure [(Clean, f) | f <- files]
            else do
                pfx <- T.strip <$> readProc "git" ["rev-parse", "--show-prefix"]
                u <- Set.fromList . filter (not . T.null) . T.lines <$> readProc "git" ["diff", "--name-only"]
                s <- Set.fromList . filter (not . T.null) . T.lines <$> readProc "git" ["diff", "--cached", "--name-only"]
                tr <- Set.fromList . filter (not . T.null) . T.lines <$> readProc "git" ["ls-files", "--others", "--exclude-standard"]
                let classifyFile f =
                        let rp = pfx <> f
                         in case () of
                                _
                                    | Set.member rp u -> Unstaged
                                    | Set.member rp s -> Staged
                                    | Set.member rp tr -> Untracked
                                    | otherwise -> Clean
                    anyUnder st dir' =
                        let d = T.dropWhileEnd (== '/') dir'
                            dp = pfx <> d <> "/"
                         in any (dp `T.isPrefixOf`) (Set.toList st)
                    classifyDir d =
                        case () of
                            _
                                | anyUnder u d -> Unstaged
                                | anyUnder s d -> Staged
                                | anyUnder tr d -> Untracked
                                | otherwise -> Clean
                    classify f = if cFd == FdDirs then classifyDir f else classifyFile f
                pure [(classify f, f) | f <- files]
    let statusOk = filter (\(st, _) -> maybe True (== st) sf) labeled
        ok = if cGitSt then filter (\(st, _) -> st /= Clean) statusOk else statusOk
        (top, bot) = partition (\(_, f) -> "src/" `T.isPrefixOf` f) ok
        dotEntry
            | cFd /= FdDirs = []
            | any (\(st, _) -> st == Unstaged) labeled = [(Unstaged, ".")]
            | any (\(st, _) -> st == Staged) labeled = [(Staged, ".")]
            | any (\(st, _) -> st == Untracked) labeled = [(Untracked, ".")]
            | otherwise = [(Clean, ".")]
        final = dotEntry <> top <> bot
        -- Compute positions of saved selections for restore
        savedSel = case cFd of
            FdFiles -> Set.fromList cSavedFileSel
            FdDirs -> Set.fromList cSavedDirSel
    cwd <- T.pack <$> getCurrentDirectory
    let toAbs f = if isAbsolute (t f) then f else T.pack (t cwd </> t f)
        positions = [showT i | (i, (_, f)) <- zip [1 :: Int ..] final, Set.member (toAbs f) savedSel]
    unless (null positions) $ do
        let posFile = t cDir </> "sel-positions"
        writeFile posFile (T.unpack (T.unlines positions))
        -- Clear saved selections after computing positions
        case cFd of
            FdFiles -> void $ modConfig $ \x -> x{cSavedFileSel = []}
            FdDirs -> void $ modConfig $ \x -> x{cSavedDirSel = []}
    -- Write stats for preview sizing
    let maxW = if null final then 0 else maximum [T.length f + 2 | (_, f) <- final]
    writeFile (t cDir </> "max-width") (show maxW)
    writeFile (t cDir </> "line-count") (show (length final))
    mapM_ (\(st, f) -> TIO.putStrLn (gitStatusChar st <> "\t" <> f)) final
    hFlush stdout

loadFzfxInclude :: Text -> IO [Text]
loadFzfxInclude ty = do
    let includeFile = ".fzfxinclude"
    exists <- doesFileExist includeFile
    if not exists
        then pure []
        else do
            content <- T.pack <$> readFile' includeFile
            let paths = filter (\l -> not (T.null l) && not ("#" `T.isPrefixOf` l)) $ map T.strip $ T.lines content
            if null paths
                then pure []
                else do
                    let fa =
                            [ "--type"
                            , ty
                            , "--no-ignore"
                            , "--hidden"
                            , "-L"
                            , "--exclude"
                            , ".git"
                            ]
                                <> concatMap (\p -> ["--search-path", p]) paths
                    let strip f = fromMaybe f (T.stripPrefix "./" f)
                    catch
                        (map strip . filter (not . T.null) . T.lines <$> readProc "fd" fa)
                        (\(_ :: IOException) -> pure [])

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
    filtered <-
        if T.null filt
            then pure files
            else do
                let input = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines files
                (_, fout, _) <- readProcess (setStdin (byteStringInput input) $ proc "fzf" ["--filter", t filt])
                let fout' = decodeOut fout
                pure $ filter (not . T.null) (T.lines fout')
    mapM_ (\f -> TIO.putStrLn (" \t" <> f)) filtered
    hFlush stdout

reloadFzfRg :: Text -> Text -> [Text] -> IO ()
reloadFzfRg filt rgPat ex = unless (T.null rgPat) $ do
    -- Get all files, filter with fzf, then rg within matches
    fdOut <- readProc "fd" ["--type", "f", "--exclude", ".git", "--exclude", "node_modules", "--strip-cwd-prefix"]
    let allFiles = filter (not . T.null) (T.lines fdOut)
    targets <-
        if T.null filt
            then pure allFiles
            else do
                let input = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines allFiles
                (_, fout, _) <- readProcess (setStdin (byteStringInput input) $ proc "fzf" ["--filter", t filt])
                let fout' = decodeOut fout
                pure $ filter (not . T.null) (T.lines fout')
    unless (null targets) $ do
        let rgArgs =
                ["--column", "--line-number", "--no-heading", "--color=always", "--smart-case"]
                    <> ex
                    <> ["--", rgPat]
                    <> targets
        runProcess_ $ setStdout inherit $ setStderr nullStream $ proc "rg" (map t rgArgs)

reloadFzfRgPending :: Text -> IO ()
reloadFzfRgPending filt = do
    fdOut <- readProc "fd" ["--type", "f", "--exclude", ".git", "--exclude", "node_modules", "--strip-cwd-prefix"]
    let allFiles = filter (not . T.null) (T.lines fdOut)
    filtered <-
        if T.null filt
            then pure allFiles
            else do
                let input = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines allFiles
                (_, fout, _) <- readProcess (setStdin (byteStringInput input) $ proc "fzf" ["--filter", t filt])
                let fout' = decodeOut fout
                pure $ filter (not . T.null) (T.lines fout')
    mapM_ (\f -> TIO.putStrLn (" \t" <> f)) filtered
    hFlush stdout

showSymlink :: Text -> IO ()
showSymlink path = do
    isSym <- pathIsSymbolicLink (t path) `catch` \(_ :: IOException) -> pure False
    when isSym $ do
        target <- T.pack <$> getSymbolicLinkTarget (t path)
        TIO.putStrLn $ "\ESC[36m→ " <> target <> "\ESC[0m"
        hFlush stdout

cmdPreview :: Text -> IO ()
cmdPreview line = withCfg $ \Config{..} -> do
    rows <- maybe 40 readInt <$> lookupEnv "FZF_PREVIEW_LINES"
    if T.null (T.strip line)
        then contentPrev (not (T.null cGit)) "."
        else case parseLine line of
            RgLine file ln -> do
                showSymlink file
                let start = max 1 (ln - rows `div` 2)
                exec "bat" ["--color=always", "--style=numbers", "--highlight-line", showT ln, "--line-range", showT start <> ":", "--", file]
            FdLine st path
                | cPrev == Diff
                , st == Unstaged || st == Staged -> do
                    isDir <- doesDirectoryExist (t path)
                    if isDir
                        then diffPrevDir path
                        else diffPrev st path
                | otherwise -> do
                    showSymlink path
                    contentPrev (not (T.null cGit)) path
  where
    readInt s = case reads s of [(n, _)] -> n; _ -> 40 :: Int

diffArgs :: GitStatus -> Text -> [Text]
diffArgs st path = case st of
    Unstaged -> ["diff", "--", path]
    Staged -> ["diff", "--cached", "--", path]
    Untracked -> ["diff", "--no-index", "--", "/dev/null", path]
    Clean -> []

diffPrevDir :: Text -> IO ()
diffPrevDir path = do
    let args = if path == "." then ["diff"] else ["diff", "--", path]
    piped ("git", args) ("delta", [])

diffPrev :: GitStatus -> Text -> IO ()
diffPrev st path = do
    let da = diffArgs st path
    if null da
        then contentPrev True path
        else piped ("git", da) ("delta", [])

contentPrev :: Bool -> Text -> IO ()
contentPrev inGit path = do
    isDir <- doesDirectoryExist (t path)
    if isDir
        then do
            p <- if path == "." then T.pack <$> getCurrentDirectory else pure path
            let gitArgs =
                    if inGit
                        then ["-l", "--no-permissions", "--no-filesize", "--no-user", "--no-time", "--git"]
                        else []
            exec "eza" (["--icons", "--git-ignore", "--tree", "-L", "3", "--color=always"] <> gitArgs <> [p])
        else
            if takeExtension (t path) == ".ipynb"
                then exec "nbpreview" [path]
                else exec "bat" [path, "--style=plain", "--color=always", "--line-range", "0:100"]

cmdFullPreview :: Text -> IO ()
cmdFullPreview line = withCfg $ \Config{..} -> do
    tmp <- getTemporaryDirectory
    let lkFile = tmp </> "fzfx-lesskey"
    writeFile lkFile "#command\n\\e\\e quit\n\\n quit\n\\r quit\n^G quit\n\\e/ quit\n^X^S quit\n"
    case parseLine line of
        RgLine file ln ->
            let pager = "less -Rc~ -j.5 +" <> show ln <> "g --lesskey-src=" <> lkFile
             in exec "bat" ["--color=always", "--style=numbers", "--highlight-line", showT ln, "--paging=always", "--pager", T.pack pager, "--", file]
        FdLine st path -> do
            let pager = "less -Rc~ --lesskey-src=" <> lkFile
            if cPrev == Diff && st /= Clean
                then do
                    let da = diffArgs st path
                    piped ("git", da) ("delta", ["--paging=always", "--pager", T.pack pager])
                else do
                    isDir <- doesDirectoryExist (t path)
                    if isDir
                        then do
                            p <- if path == "." then T.pack <$> getCurrentDirectory else pure path
                            let inGit = not (T.null cGit)
                                gitArgs =
                                    if inGit
                                        then ["-l", "--no-permissions", "--no-filesize", "--no-user", "--no-time", "--git"]
                                        else []
                            piped ("eza", ["--icons", "--git-ignore", "--tree", "-L", "3", "--color=always"] <> gitArgs <> [p]) ("less", ["-Rc~", "--lesskey-src=" <> T.pack lkFile])
                        else exec "bat" ["--color=always", "--style=plain", "--paging=always", "--pager", T.pack pager, "--", path]

queryStackDir :: IO FilePath
queryStackDir = do
    home <- envOr "HOME" "/tmp"
    pure $ t home </> ".local" </> "state" </> "fzfx"

queryStackFile :: Text -> IO FilePath
queryStackFile gitRoot = do
    dir <- queryStackDir
    let name = map (\c -> if c == pathSeparator then '_' else c) (t gitRoot)
    pure $ dir </> name

loadQueryStack :: Text -> IO [Text]
loadQueryStack gitRoot
    | T.null gitRoot = pure []
    | otherwise = do
        f <- queryStackFile gitRoot
        catch
            (filter (not . T.null) . T.lines . T.pack <$> readFile' f)
            (\(_ :: IOException) -> pure [])

saveQueryStack :: Text -> [Text] -> IO ()
saveQueryStack gitRoot stack
    | T.null gitRoot = pure ()
    | otherwise = do
        f <- queryStackFile gitRoot
        dir <- queryStackDir
        createDirectoryIfMissing True dir
        writeFile f (T.unpack (T.unlines stack))

cmdQueryPush :: Text -> IO ()
cmdQueryPush q = do
    cfg <- loadConfig
    let (cfg', _actions) = transition cfg (EvQueryPush q)
    saveConfig cfg'
    saveQueryStack (cGit cfg') (cQueryStack cfg')

-- SQueryPop has two phases:
-- Phase 1 (execute): show nested fzf, write selection to config as cPendingQuery
-- Phase 2 (transform): read cPendingQuery from config, output change-query()
cmdQueryPop :: IO ()
cmdQueryPop = do
    c <- loadConfig
    case cQueryStack c of
        [] -> pure ()
        stack -> do
            let self = t (cSelf c)
                preview = self <> " " <> t (flg SReload) <> " {} | fzf --filter {} || true"
                del =
                    "execute-silent("
                        <> self
                        <> " "
                        <> t (flg SQueryDelete)
                        <> " {})"
                        <> "+reload-sync("
                        <> self
                        <> " "
                        <> t (flg SQueryList)
                        <> ")"
                fzfProc =
                    setStdin (byteStringInput (LBS.fromStrict (TE.encodeUtf8 (T.intercalate "\n" stack)))) $
                        setStdout createPipe $
                            proc
                                (t "fzf")
                                [ "--ansi"
                                , "--reverse"
                                , "--no-multi"
                                , "--prompt=query> "
                                , "--header=Select a saved query (ctrl-d to delete)"
                                , "--preview=" <> preview
                                , "--preview-window=bottom,70%"
                                , "--bind=ctrl-g:abort"
                                , "--bind=ctrl-d:" <> del
                                ]
            withProcessWait fzfProc $ \p -> do
                out <- LBS.hGetContents (getStdout p)
                ec <- waitExitCode p
                case ec of
                    ExitSuccess -> do
                        let sel = T.strip (decodeOut out)
                        void $ modConfig $ \x -> x{cPendingQuery = sel}
                    _ -> pure ()

cmdQueryApply :: IO ()
cmdQueryApply = do
    c <- loadConfig
    let sel = cPendingQuery c
    unless (T.null sel) $ do
        void $ modConfig $ \x -> x{cPendingQuery = ""}
        let rl = "reload-sync(" <> cSelf c <> " " <> flg SReload <> " " <> sel <> ")"
        TIO.putStr $ rl <> "+change-query(" <> sel <> ")"
        hFlush stdout

cmdQueryDelete :: Text -> IO ()
cmdQueryDelete q = do
    cfg <- loadConfig
    let (cfg', _actions) = transition cfg (EvQueryDelete q)
    saveConfig cfg'
    saveQueryStack (cGit cfg') (cQueryStack cfg')

cmdQueryList :: IO ()
cmdQueryList = do
    c <- loadConfig
    mapM_ TIO.putStrLn (cQueryStack c)
    hFlush stdout

{- | Save current selections for a mode (file/dir).
Mode passed as arg, paths read from stdin (newline-separated).
-}
cmdSelSave :: Text -> IO ()
cmdSelSave mode = do
    input <- TIO.getContents
    cwd <- T.pack <$> getCurrentDirectory
    let paths = filter (not . T.null) $ T.lines input
        absPaths = map (\p -> if isAbsolute (t p) then p else T.pack (t cwd </> t p)) paths
    case mode of
        "file" -> void $ modConfig $ \x -> x{cSavedFileSel = absPaths}
        "dir" -> void $ modConfig $ \x -> x{cSavedDirSel = absPaths}
        _ -> pure ()

{- | Restore saved selections after a mode switch reload.
Called from load event transform — reads pending positions file and emits pos()+select actions.
-}
cmdSelRestore :: IO ()
cmdSelRestore = do
    c <- loadConfig
    let posFile = t (cDir c) </> "sel-positions"
    positions <-
        catch
            (filter (not . T.null) . T.lines . T.pack <$> readFile' posFile)
            (\(_ :: IOException) -> pure [])
    unless (null positions) $ do
        writeFile posFile ""
        let actions = T.intercalate "+" $ concatMap (\p -> ["pos(" <> p <> ")", "select"]) positions
        TIO.putStr actions
        hFlush stdout

cmdSmartEnter :: Text -> IO ()
cmdSmartEnter args = do
    cfg <- loadConfig
    let (_cfg', actions) = transition cfg (EvSmartEnter (args == "--alt"))
    TIO.putStr (renderActions actions)
    hFlush stdout

cmdEdit :: Text -> IO ()
cmdEdit line = withCfg $ \_ -> do
    reopenTty
    case parseLine line of
        RgLine f ln -> executeFile "tr-edit" True [t ("+" <> showT ln), t f] Nothing
        FdLine _ p -> executeFile "tr-edit" True [t p] Nothing

{- | Restore stdin/stdout to the terminal for become: handlers.
fzfx pipes fzf's stdin (reload data) and stdout (selection capture),
but stderr is inherited as the real pty throughout fzfx → fzf → become.
Dup stderr over the piped fds so child processes get a proper terminal.
-}
reopenTty :: IO ()
reopenTty = do
    _ <- dupTo stdError stdInput
    void $ dupTo stdError stdOutput

cmdPreviewWidth :: IO ()
cmdPreviewWidth = withCfg $ \Config{..} -> do
    let widthFile = t cDir </> "max-width"
    exists <- doesFileExist widthFile
    when exists $ do
        content <- readFile' widthFile
        let maxW = case reads content of [(n, _)] -> n; _ -> 0 :: Int
            readInt' s = case reads s of [(n, _)] -> n; _ -> 0 :: Int
        case cPreviewLayout of
            PrevRight -> do
                fzfCols <- lookupEnv "FZF_COLUMNS"
                stdCols <- lookupEnv "COLUMNS"
                let termW = case fzfCols of
                        Just s | readInt' s > 0 -> readInt' s
                        _ -> case stdCols of
                            Just s | readInt' s > 0 -> readInt' s
                            _ -> 120
                    listNeed = maxW + 4
                    prevPct = max 50 (((termW - listNeed) * 100) `div` termW)
                TIO.putStr $ "change-preview-window(right:" <> showT prevPct <> "%)"
            PrevBottom -> do
                let lcFile = t cDir </> "line-count"
                lcExists <- doesFileExist lcFile
                when lcExists $ do
                    lcContent <- readFile' lcFile
                    let lineCount = case reads lcContent of [(n, _)] -> n; _ -> 0 :: Int
                    fzfLines <- lookupEnv "FZF_LINES"
                    stdLines <- lookupEnv "LINES"
                    let termH = case fzfLines of
                            Just s | readInt' s > 0 -> readInt' s
                            _ -> case stdLines of
                                Just s | readInt' s > 0 -> readInt' s
                                _ -> 40
                        -- File list needs: results + header (~3 lines) + prompt
                        listNeed = lineCount + 4
                        prevPct = max 50 (((termH - listNeed) * 100) `div` termH)
                    TIO.putStr $ "change-preview-window(bottom:" <> showT prevPct <> "%)"
    hFlush stdout

cmdDebug :: IO ()
cmdDebug = withCfg $ \cfg -> do
    reopenTty
    let args = fzfArgs cfg
        self = cSelf cfg
        abbrev = "fzfx"
        content = T.unlines $ toggleSummary cfg <> ["", "# fzfArgs  (fzfx=" <> self <> ")", ""] <> map (formatArg self abbrev) args <> ["", "# config", ""] <> prettyConfig cfg
    runProcess_ $
        setStdin (byteStringInput (LBS.fromStrict (TE.encodeUtf8 content))) $
            proc "less" ["-R"]
  where
    formatArg self abbrev a = case T.breakOn "=" (T.replace self abbrev a) of
        (key, val)
            | "--bind" `T.isPrefixOf` key ->
                let (bkey, bval) = T.breakOn ":" (T.drop 1 val)
                 in "  " <> dim key <> "=" <> bold bkey <> prettifyBash bval
            | not (T.null val) -> "  " <> dim key <> "=" <> T.drop 1 val
            | otherwise -> "  " <> a
    bold s = "\ESC[1;33m" <> s <> "\ESC[0m"
    dim s = "\ESC[2m" <> s <> "\ESC[0m"
    toggleSummary Config{..} =
        let on label = "\ESC[1;32m" <> label <> "\ESC[0m"
            off label = "\ESC[2m" <> label <> "\ESC[0m"
         in [ "# toggles"
            , ""
            , "  " <> bold "M-a" <> "  @ prefix    " <> if cAt then on "ON" else off "off"
            , "  "
                <> bold "C-/"
                <> "  type         "
                <> (if cFd == FdDirs then on "dirs" else off "dirs")
                <> " / "
                <> (if cFd == FdFiles then on "files" else off "files")
            , "  " <> bold "M-h" <> "  hidden       " <> if cHid then on "ON" else off "off"
            , "  " <> bold "M-i" <> "  no-ignore    " <> if cIgn then on "ON" else off "off"
            , "  "
                <> bold "M-g"
                <> "  preview      "
                <> (if cPrev == Diff then on "diff" else off "diff")
                <> " / "
                <> (if cPrev == Content then on "content" else off "content")
            , "  " <> bold "M-a" <> "  output       " <> (case cOut of OTmux -> on "tmux"; OStdout -> on "stdout")
            ]
    prettifyBash s =
        foldl
            (\acc (pat, rep) -> T.replace pat rep acc)
            s
            [ ("; then ", ";\n      then ")
            , ("; elif ", ";\n      elif ")
            , ("; else ", ";\n      else ")
            , ("; fi", ";\n      fi")
            , (";;", ";;\n      ")
            , ("; esac", ";\n      esac")
            ]
    prettyConfig Config{..} =
        let fields =
                [ ("cDir", "state_dir", cDir)
                , ("cGit", "git_root", cGit)
                , ("cOrig", "orig_cwd", cOrig)
                , ("cCwd", "cwd", cCwd)
                , ("cPane", "pane", cPane)
                , ("cSelf", "self", cSelf)
                , ("cQuery", "query", cQuery)
                , ("cOut", "output_mode", showT cOut)
                , ("cAt", "at_prefix", showT cAt)
                , ("cFd", "fd_type", showT cFd)
                , ("cHid", "hidden", showT cHid)
                , ("cIgn", "no_ignore", showT cIgn)
                , ("cPrev", "preview_mode", showT cPrev)
                ]
            pad n s = s <> T.replicate (max 0 (n - T.length s)) " "
         in map (\(f, desc, v) -> pad 7 f <> pad 14 desc <> " = " <> v) fields

cmdSwap :: Text -> IO ()
cmdSwap q = do
    cfg <- loadConfig
    let (_cfg', actions) = transition cfg (EvSwap q)
    TIO.putStr (renderActions actions)
    hFlush stdout

cmdExtraArgs :: Text -> IO ()
cmdExtraArgs q = do
    cfg <- loadConfig
    let (_cfg', actions) = transition cfg (EvExtraArgs q)
    unless (null actions) $ TIO.putStr (renderActions actions)
    hFlush stdout

cmdMagit :: Text -> IO ()
cmdMagit line = withCfg $ \_ ->
    exec "magit-file-status" [lineFile (parseLine line)]

cmdForgit :: Text -> IO ()
cmdForgit line = withCfg $ \_ ->
    exec "git-forgit" ["log", "--", lineFile (parseLine line)]

cmdTokei :: Text -> IO ()
cmdTokei line = withCfg $ \Config{..} -> do
    reopenTty
    let dir = case parseLine line of
            FdLine _ p ->
                let fp = t cCwd </> t p
                 in T.pack $ takeDirectory fp
            RgLine f _ -> T.pack $ takeDirectory (t f)
    piped ("tokei", [dir]) ("less", ["-R"])

cmdCopy :: Text -> IO ()
cmdCopy line = do
    let path = T.stripEnd $ lineFile (parseLine line)
    runProcess_ $
        setStdin (byteStringInput (LBS.fromStrict (TE.encodeUtf8 path))) $
            proc "tmux" ["load-buffer", "-w", "-"]

cmdTransform :: Text -> IO ()
cmdTransform q = withCfg $ \cfg -> do
    let (cfg', actions) = transition cfg (EvTransform q)
    saveConfig cfg'
    TIO.putStr (renderActions actions)
    hFlush stdout

cmdToggle :: Text -> IO ()
cmdToggle nameAndArgs = do
    cfg <- loadConfig
    let (name, curQ) = case T.breakOn " " nameAndArgs of
            (n, rest) -> (n, T.drop 1 rest)
        tgName = case name of
            "at_prefix" -> TgAtPrefix
            "diff" -> TgDiff
            "hidden" -> TgHidden
            "no_ignore" -> TgNoIgnore
            "git_status" -> TgGitStatus
            "preview_layout" -> TgPreviewLayout
            "type_toggle" -> TgType
            "type_d" -> TgTypeD
            "type_f" -> TgTypeF
            _ -> TgAtPrefix -- fallback, no-op if already matching
        (cfg', actions) = transition cfg (EvToggle tgName curQ)
    unless (null actions) $ do
        saveConfig cfg'
        TIO.putStr (renderActions actions)
        hFlush stdout

cmdNavigate :: Text -> Text -> Text -> IO ()
cmdNavigate navAction sel query = do
    Config{..} <- loadConfig
    let sp = T.dropWhileEnd (== pathSeparator) $ lineFile (parseLine sel)
    nCwd <- case navAction of
        "into" -> do
            let target = t cCwd </> t sp
            d <- doesDirectoryExist target
            pure $
                T.pack $
                    if d
                        then target
                        else takeDirectory target
        "into_top" -> do
            let topDir = case splitDirectories (t sp) of
                    (d : _) -> d
                    _ -> ""
                target = t cCwd </> topDir
            d <- doesDirectoryExist target
            pure $
                if d && not (null topDir)
                    then T.pack target
                    else cCwd
        "up" -> pure $ T.pack (takeDirectory (t cCwd))
        "root" -> do
            origGit <- detectGit cOrig
            pure $ if T.null origGit then cOrig else origGit
        "toggle_root" -> pure cOrig
        _ -> pure cCwd
    let nCwd' = T.dropWhileEnd (== pathSeparator) $ T.pack $ FP.normalise $ t nCwd
    nGit <- detectGit nCwd'
    let nFd = if navAction == "into" then FdFiles else cFd
        -- Dir segments that will disappear after navigation
        vanishingDirs = case navAction of
            "into" -> map T.pack $ splitDirectories (t sp)
            "into_top" -> case splitDirectories (t sp) of (d : _) -> [T.pack d]; _ -> []
            _ -> []
    -- Use fzf --filter to check each query chunk against vanishing dir segments
    baseQuery <-
        if null vanishingDirs || T.null query
            then pure query
            else do
                let dirInput = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines vanishingDirs
                    chunks = T.words query
                    checkChunk chunk = do
                        (ec, out, _) <-
                            readProcess $
                                setStdin (byteStringInput dirInput) $
                                    proc "fzf" ["--filter", t chunk, "--no-sort"]
                        pure $ case ec of
                            ExitSuccess -> not $ LBS.null out -- matches found → chunk matches dir
                            _ -> False
                matches <- mapM (\c -> (c,) <$> checkChunk c) chunks
                pure $ T.unwords [c | (c, matched) <- matches, not matched]
    let
        nQuery = if navAction == "into" && cFd == FdDirs then cFileQuery else baseQuery
        nFileQ = cFileQuery
        nDirQ = if navAction == "into" && cFd == FdDirs then query else cDirQuery
    setEnv "_FZFX_CWD" (t nCwd')
    setEnv "_FZFX_QUERY" (t nQuery)
    setEnv "_FZFX_AT_PREFIX" (if cAt then "1" else "0")
    setEnv "_FZFX_FDTYPE" (case nFd of FdFiles -> "f"; FdDirs -> "d")
    setEnv "_FZFX_HIDDEN" (if cHid then "--hidden" else "")
    -- Auto-disable git-status when navigating to a dir with no dirty files
    nGitSt <-
        if not cGitSt || T.null nGit
            then pure False
            else do
                out <- readProcMaybe "git" ["-C", nCwd', "status", "--porcelain"]
                pure $ maybe False (not . T.null . T.strip) out
    let nPrev = if cGitSt && not nGitSt then Content else cPrev
    setEnv "_FZFX_GIT_STATUS" (if nGitSt then "1" else "0")
    setEnv "_FZFX_PREV_MODE" (if nPrev == Diff then "diff" else "content")
    setEnv "_FZFX_PREVIEW" (if cPreviewOn then "1" else "0")
    setEnv "_FZFX_PREVIEW_LAYOUT" (case cPreviewLayout of PrevRight -> "right"; PrevBottom -> "bottom")
    setEnv "_FZFX_FILE_QUERY" (t nFileQ)
    setEnv "_FZFX_DIR_QUERY" (t nDirQ)
    setEnv "_FZFX_SAVED_FILE_SEL" (t (T.intercalate "\n" cSavedFileSel))
    setEnv "_FZFX_SAVED_DIR_SEL" (t (T.intercalate "\n" cSavedDirSel))
    setEnv "_FZFX_STATE_DIR" ""
    mainLaunch defaultRunOpts

cmdZoxide :: Text -> IO ()
cmdZoxide _query = do
    Config{..} <- loadConfig
    reopenTty
    (ec, out, _) <- readProcess $ proc "zoxide" ["query", "--interactive"]
    case ec of
        ExitSuccess -> do
            let nCwd = T.strip (decodeOut out)
            unless (T.null nCwd) $ do
                setEnv "_FZFX_CWD" (t nCwd)
                setEnv "_FZFX_QUERY" ""
                setEnv "_FZFX_AT_PREFIX" (if cAt then "1" else "0")
                setEnv "_FZFX_FDTYPE" (case cFd of FdFiles -> "f"; FdDirs -> "d")
                setEnv "_FZFX_HIDDEN" (if cHid then "--hidden" else "")
                setEnv "_FZFX_GIT_STATUS" (if cGitSt then "1" else "0")
                setEnv "_FZFX_PREV_MODE" (if cPrev == Diff then "diff" else "content")
                setEnv "_FZFX_PREVIEW" (if cPreviewOn then "1" else "0")
                setEnv "_FZFX_PREVIEW_LAYOUT" (case cPreviewLayout of PrevRight -> "right"; PrevBottom -> "bottom")
                setEnv "_FZFX_FILE_QUERY" ""
                setEnv "_FZFX_DIR_QUERY" ""
                setEnv "_FZFX_SAVED_FILE_SEL" ""
                setEnv "_FZFX_SAVED_DIR_SEL" ""
                setEnv "_FZFX_STATE_DIR" ""
                mainLaunch defaultRunOpts
        _ -> pure ()

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

{- | Prefix that saves current tab-selections to config before an action.
Uses FZF_SELECT_COUNT to avoid saving the cursor item when nothing is selected.
-}
selSavePrefix :: Config -> Text
selSavePrefix Config{..} =
    let mode = case cFd of FdFiles -> "file"; FdDirs -> "dir"
     in "execute-silent([ \"$FZF_SELECT_COUNT\" -gt 0 ] && printf '%s\\n' {+2} | "
            <> cSelf
            <> " "
            <> flg SSelSave
            <> " "
            <> mode
            <> ")+"

bc :: Config -> Text -> Subcmd -> Text -> Text
bc cfg@Config{..} key sub rest =
    bind key $ selSavePrefix cfg <> "become:" <> cSelf <> " " <> flg sub <> " " <> rest

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
        , "--tabstop=4"
        , "--with-nth=1,2"
        , "--id-nth=2"
        , "--border=bottom"
        , "--border-label=" <> hdrText cfg
        , "--border-label-pos=bottom"
        , "--prompt=" <> (if cFd == FdDirs then "dirs" else "files") <> "> "
        , "--query=" <> cQuery
        , "--preview=" <> cSelf <> " " <> flg SPreview <> " {}"
        , "--preview-window="
            <> (case cPreviewLayout of PrevRight -> "right"; PrevBottom -> "bottom")
            <> ":50%"
            <> (if cPreviewOn then "" else ":hidden")
        ]
    selfBindings =
        [ xf cfg "change" STransform "{q}"
        , xf cfg "alt-a" SToggle "at_prefix"
        , xe cfg "alt-/" SFullPreview "{}" ""
        , xf cfg "alt-g" SToggle "git_status"
        , bind "alt-u" (statusToggle "U")
        , bind "alt-s" (statusToggle "S")
        , bind "alt-?" (statusToggle "?")
        , xf cfg "alt-h" SToggle "hidden"
        , xf cfg "alt-i" SToggle "no_ignore"
        , bind "ctrl-t" (selSavePrefix cfg <> "transform:" <> cSelf <> " " <> flg SToggle <> " type_toggle {q}")
        , bc cfg "ctrl-o" SNavigate "into {} {q}"
        , bc cfg "alt-o" SNavigate "into_top {} {q}"
        , bc cfg "alt-l" SNavigate "into_top {} {q}"
        , bc cfg "ctrl-l" SNavigate "up {} {q}"
        , bc cfg "ctrl-r" SNavigate "root {} {q}"
        , bc cfg "alt-." SNavigate "toggle_root {} {q}"
        , bc cfg "alt-z" SZoxide "{q}"
        , bind "enter" (selSavePrefix cfg <> "transform:" <> cSelf <> " " <> flg SSmartEnter)
        , bind "alt-enter" (selSavePrefix cfg <> "transform:" <> cSelf <> " " <> flg SSmartEnter <> " --alt")
        , xe cfg "alt-t" STokei "{}" ""
        , xe cfg "alt-," SMagit "{}" "+abort"
        , xe cfg "alt-c" SCopy "{}" "+abort"
        , xe cfg "ctrl-alt-l" SForgit "{}" ""
        , xe cfg "ctrl-alt-d" SDebug "" ""
        , xf cfg "ctrl-p" SToggle "preview_layout"
        , xf cfg "alt-r" SSwap "{q}"
        ]
    staticBindings =
        [ bind "tab" "toggle+down"
        , bind "shift-tab" "toggle+down+end-of-line+unix-line-discard"
        , bind "ctrl-k" "kill-line"
        , bind "alt-k" "clear-query"
        , xf cfg "alt--" SExtraArgs "{q}"
        , bind "alt-3" "transform:q={q}; case $q in \\#*) ;; *) printf change-query\\(\\#{q}\\)+beginning-of-line+forward-char;; esac"
        , bind "alt-b" "backward-word"
        , bind "alt-f" "forward-word"
        , bind "alt-p" "toggle-preview"
        , bind "alt-;" "change-preview-window(up,99%,border-bottom|right,50%,border-left)"
        , bind "alt-{" "preview-half-page-up"
        , bind "alt-}" "preview-half-page-down"
        , bind "alt-space" "preview-page-down"
        , bind "ctrl-space" "preview-page-up"
        , xf cfg "ctrl-alt-g" SToggle "diff"
        , bind "ctrl-g" "abort"
        , bind "ctrl-z" "abort"
        , xf cfg "f4" SQueryPush "{q}"
        , xe cfg "f3" SQueryPop "" ("+transform:" <> cSelf <> " " <> flg SQueryApply)
        , bind "zero" ("preview(" <> cSelf <> " " <> flg SPreview <> ")")
        , bind "result" ("transform:" <> cSelf <> " " <> flg SPreviewWidth)
        , bind "load" ("transform:" <> cSelf <> " " <> flg SSelRestore)
        ]

-- ═══════════════════════════════════════════════════════════════════════
-- Output
-- ═══════════════════════════════════════════════════════════════════════

outputResults :: Config -> [Text] -> IO ()
outputResults Config{..} sel = do
    let
        -- Lines with tabs are raw fzf output (need resolving via makeRelPath).
        -- Lines without tabs came from an inner mainLaunch via become: chain
        -- and are already resolved — pass them through unchanged.
        isFzfLine = T.isInfixOf "\t"
        resolve line
            | isFzfLine line = case parseLine line of
                RgLine f ln -> makeRelPath cOrig cCwd f <> ":" <> showT ln
                FdLine _ p -> makeRelPath cOrig cCwd p
            | otherwise = line
        files = dedup $ sort $ map resolve sel
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

-- ── CLI options ──────────────────────────────────────────────────────

data RunOpts = RunOpts
    { optCwd :: !(Maybe Text)
    , optOutput :: !(Maybe OutMode)
    , optType :: !(Maybe FdType)
    , optHidden :: !Bool
    , optAtPrefix :: !Bool
    , optGitStatus :: !Bool
    , optPreview :: !(Maybe Bool)
    , optPreviewLayout :: !(Maybe PreviewLayout)
    , optPane :: !(Maybe Text)
    , optQuery :: ![Text]
    }

-- | All-default opts for internal re-launch (state comes from env vars)
defaultRunOpts :: RunOpts
defaultRunOpts = RunOpts Nothing Nothing Nothing False False False Nothing Nothing Nothing []

parseOutMode :: ReadM OutMode
parseOutMode = eitherReader $ \case
    "stdout" -> Right OStdout
    "tmux" -> Right OTmux
    s -> Left $ "unknown output mode: " <> s <> " (expected stdout|tmux)"

parsePreviewLayout :: ReadM PreviewLayout
parsePreviewLayout = eitherReader $ \case
    "right" -> Right PrevRight
    "bottom" -> Right PrevBottom
    s -> Left $ "unknown layout: " <> s <> " (expected right|bottom)"

parseFdType :: ReadM FdType
parseFdType = eitherReader $ \case
    "f" -> Right FdFiles
    "d" -> Right FdDirs
    "files" -> Right FdFiles
    "dirs" -> Right FdDirs
    s -> Left $ "unknown type: " <> s <> " (expected f|d|files|dirs)"

runOptsParser :: Parser RunOpts
runOptsParser =
    RunOpts
        <$> optional
            ( T.pack
                <$> strOption
                    ( long "cwd"
                        <> short 'C'
                        <> metavar "DIR"
                        <> help "Starting directory"
                    )
            )
        <*> optional
            ( option
                parseOutMode
                ( long "output"
                    <> short 'o'
                    <> metavar "MODE"
                    <> help "Output mode (stdout|tmux)"
                )
            )
        <*> optional
            ( option
                parseFdType
                ( long "type"
                    <> short 't'
                    <> metavar "TYPE"
                    <> help "File type filter (f|d|files|dirs)"
                )
            )
        <*> switch
            ( long "hidden"
                <> short 'H'
                <> help "Show hidden files"
            )
        <*> switch
            ( long "at-prefix"
                <> help "Prefix output with @"
            )
        <*> switch
            ( long "git-status"
                <> short 'g'
                <> help "Only show files with git status (dirty files)"
            )
        <*> optional
            ( flag' True (long "preview" <> help "Enable preview (default)")
                <|> flag' False (long "no-preview" <> help "Disable preview")
            )
        <*> optional
            ( option
                parsePreviewLayout
                ( long "preview-layout"
                    <> metavar "LAYOUT"
                    <> help "Preview position (right|bottom)"
                )
            )
        <*> optional
            ( T.pack
                <$> strOption
                    ( long "pane"
                        <> short 'p'
                        <> metavar "PANE"
                        <> help "Tmux target pane"
                    )
            )
        <*> many
            ( T.pack
                <$> strArgument
                    ( metavar "QUERY..."
                        <> help "Initial search query"
                    )
            )

keybindingsHelp :: String
keybindingsHelp =
    unlines
        [ "Keybindings:"
        , "  enter           select file(s) and output/insert"
        , "  alt-enter       open in editor (tr-edit)"
        , "  tab             toggle selection down"
        , "  shift-tab       toggle + clear line"
        , "  alt-3           switch to ripgrep mode"
        , "  alt-a           toggle @ prefix on output"
        , "  alt-r           swap query/results"
        , "  alt-/           full-screen preview in less"
        , "  alt-g           toggle git status filter"
        , "  ctrl-alt-g      toggle diff preview"
        , "  alt-t           tokei stats for selection dir"
        , "  alt-c           copy path to tmux buffer"
        , "  alt-,           magit file status"
        , "  ctrl-alt-l      forgit log"
        , "  ctrl-o          navigate into directory"
        , "  alt-o/l         navigate into directory (top)"
        , "  ctrl-l          navigate up"
        , "  ctrl-r          navigate to git root"
        , "  alt-.           go to original directory"
        , "  alt-z           zoxide jump (interactive)"
        , "  alt-h           show/hide hidden files"
        , "  alt-i           toggle no-ignore"
        , "  alt--           extra args for fd/rg"
        , "  ctrl-t          toggle files/dirs"
        , "  alt-u/s/?       filter by git status"
        , "  ctrl-p          toggle preview layout (right/bottom)"
        , "  alt-p           toggle preview"
        , "  alt-;           cycle preview layout"
        , "  alt-{/}         preview half-page up/down"
        , "  alt-space       preview page down"
        , "  ctrl-space      preview page up"
        , "  f4              push query to stack"
        , "  f3              browse/restore query stack"
        , "  alt-k           clear query"
        , "  ctrl-k          kill line"
        , "  alt-b/f         backward/forward word"
        , "  ctrl-g/z        abort"
        , ""
        , "Query syntax:"
        , "  <text>          file search (fd)"
        , "  #<pattern>      live ripgrep search"
        , "  #<pat>#<filter> ripgrep then filter results"
        , "  \\#<text>        literal # in file search"
        , ""
        , "Environment (overridden by flags):"
        , "  _FZFX_CWD         starting directory        (--cwd)"
        , "  _FZFX_OUTPUT_MODE output mode               (--output)"
        , "  _FZFX_FDTYPE      file type filter           (--type)"
        , "  _FZFX_HIDDEN      show hidden files          (--hidden)"
        , "  _FZFX_PANE        tmux target pane           (--pane)"
        , "  _FZFX_ORIG_CWD    original cwd (toggle_root)"
        , "  _FZFX_QUERY       initial query"
        , "  _FZFX_AT_PREFIX   @ prefix (0/1)            (--at-prefix)"
        , "  _FZFX_GIT_STATUS  git status filter (0/1)   (--git-status)"
        , "  _FZFX_PREVIEW      preview on/off (0/1)     (--preview/--no-preview)"
        , "  _FZFX_PREVIEW_LAYOUT  preview position       (--preview-layout)"
        , ""
        , "Internal (set automatically during re-launch):"
        , "  _FZFX_STATE_DIR      temp state directory"
        , "  _FZFX_FILE_QUERY     saved file-mode query"
        , "  _FZFX_DIR_QUERY      saved dir-mode query"
        , "  _FZFX_SAVED_FILE_SEL saved file selections"
        , "  _FZFX_SAVED_DIR_SEL  saved dir selections"
        , "  ctrl-alt-d           debug info"
        ]

optsInfo :: ParserInfo RunOpts
optsInfo =
    info
        (runOptsParser <**> helper)
        ( fullDesc
            <> progDesc "FZF file picker with ripgrep integration"
            <> header "fzfx - FZF file picker with ripgrep integration"
            <> footerDoc (Just (pretty keybindingsHelp))
        )

-- ── Main ─────────────────────────────────────────────────────────────

main :: IO ()
main = do
    args <- getArgs
    case args of
        (cmd : rest) | Just sub <- parseSubcmd cmd -> dispatch sub rest
        _ -> mainLaunch =<< execParser optsInfo

mainLaunch :: RunOpts -> IO ()
mainLaunch RunOpts{..} = do
    self <- T.pack <$> getExecutablePath
    cwd <- case optCwd of
        Just d -> pure d
        Nothing -> envOrM "_FZFX_CWD" (T.pack <$> getCurrentDirectory)
    orig <- envOr "_FZFX_ORIG_CWD" cwd
    inTmux <- maybe False (not . null) <$> lookupEnv "TMUX"
    pane <- case optPane of
        Just p -> pure p
        Nothing ->
            if not inTmux
                then pure ""
                else
                    lookupEnv "_FZFX_PANE" >>= \case
                        Just p | not (null p) -> pure (T.pack p)
                        _ ->
                            lookupEnv "TMUX_TARGET_PANE" >>= \case
                                Just p | not (null p) -> pure (T.pack p)
                                _ -> fromMaybe "" <$> readProcMaybe "tmux" ["display-message", "-p", "#{pane_id}"]
    om <- case optOutput of
        Just m -> pure m
        Nothing -> do
            omEnv <- envOr "_FZFX_OUTPUT_MODE" ""
            pure $ case omEnv of
                "stdout" -> OStdout
                "tmux" -> OTmux
                _ -> if T.null pane then OStdout else OTmux
    at <-
        if optAtPrefix
            then pure True
            else (== "1") <$> envOr "_FZFX_AT_PREFIX" "0"
    fd <- case optType of
        Just ty -> pure ty
        Nothing -> (\s -> if s == "d" then FdDirs else FdFiles) <$> envOr "_FZFX_FDTYPE" "f"
    hid <-
        if optHidden
            then pure True
            else T.isInfixOf "--hidden" <$> envOr "_FZFX_HIDDEN" ""
    gitSt <-
        if optGitStatus
            then pure True
            else (== "1") <$> envOr "_FZFX_GIT_STATUS" "0"
    prevMode <- do
        env <- envOr "_FZFX_PREV_MODE" ""
        pure $ case env of
            "diff" -> Diff
            "content" -> Content
            _ -> if gitSt then Diff else Content
    previewOn <- case optPreview of
        Just v -> pure v
        Nothing -> (/= "0") <$> envOr "_FZFX_PREVIEW" "1"
    previewLayout <- case optPreviewLayout of
        Just lay -> pure lay
        Nothing -> do
            env <- envOr "_FZFX_PREVIEW_LAYOUT" ""
            pure $ case env of
                "bottom" -> PrevBottom
                _ -> PrevRight
    q <-
        lookupEnv "_FZFX_QUERY" >>= \case
            Just v | not (null v) -> pure (T.pack v)
            _ -> pure (T.unwords optQuery)
    git <- detectGit cwd

    at' <-
        if not at && om == OTmux && not (T.null pane)
            then catch (detectAi pane) (\(_ :: IOException) -> pure False)
            else pure at

    tmp <- getTemporaryDirectory
    pid <- getProcessID
    fqEnv <- envOr "_FZFX_FILE_QUERY" ""
    dqEnv <- envOr "_FZFX_DIR_QUERY" ""
    fSelEnv <- envOr "_FZFX_SAVED_FILE_SEL" ""
    dSelEnv <- envOr "_FZFX_SAVED_DIR_SEL" ""
    let sd = T.pack $ tmp </> "fzfx-" <> show pid
        fq = if T.null fqEnv then (if fd == FdFiles then q else "") else fqEnv
        dq = if T.null dqEnv then (if fd == FdDirs then q else "") else dqEnv
        fSel = if T.null fSelEnv then [] else filter (not . T.null) (T.lines fSelEnv)
        dSel = if T.null dSelEnv then [] else filter (not . T.null) (T.lines dSelEnv)
    persistedStack <- loadQueryStack git
    let cfg =
            Config
                { cDir = sd
                , cGit = git
                , cOrig = orig
                , cCwd = cwd
                , cPane = pane
                , cSelf = self
                , cQuery = q
                , cOut = om
                , cAt = at'
                , cFd = fd
                , cHid = hid
                , cIgn = False
                , cPrev = prevMode
                , cFileQuery = fq
                , cDirQuery = dq
                , cQueryStack = persistedStack
                , cPendingQuery = ""
                , cWasRg = False
                , cSavedFileSel = fSel
                , cSavedDirSel = dSel
                , cGitSt = gitSt
                , cPreviewOn = previewOn
                , cPreviewLayout = previewLayout
                }

    setCurrentDirectory (t cwd)
    setEnv "_FZFX_ORIG_CWD" (t orig)

    bracket
        (saveConfig cfg >> pure sd)
        (\d -> catch (removeDirectoryRecursive (t d)) (\(_ :: IOException) -> pure ()))
        $ \_ -> do
            setEnv "_FZFX_STATE_DIR" (t sd)
            -- Pipe reload output into fzf's stdin (fzf needs real tty on stderr)
            let reloadProc =
                    setStdout createPipe $
                        proc (t self) [t (flg SReload), t q]
                fzfCmd = map t (fzfArgs cfg)
            withProcessWait_ reloadProc $ \reloadP -> do
                let fzfProc =
                        setStdin (useHandleOpen (getStdout reloadP)) $
                            setStdout createPipe $
                                proc (t "fzf") fzfCmd
                withProcessWait fzfProc $ \p -> do
                    out <- LBS.hGetContents (getStdout p)
                    ec <- waitExitCode p
                    case ec of
                        ExitSuccess -> do
                            let selected = filter (not . T.null) (T.lines (decodeOut out))
                            unless (null selected) $ do
                                c <- loadConfig
                                outputResults c selected
                        _ -> pure () -- treat abort/ctrl-c as success

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
