{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception (IOException, bracket, bracket_, catch)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (chr)
import Data.List (partition, sort)
import Data.List qualified as L
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Options.Applicative
import Options.Applicative.Help.Pretty (pretty)
import System.Directory (
    canonicalizePath,
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

import System.Exit (exitFailure)
import System.FilePath (isAbsolute, pathSeparator, splitDirectories, takeDirectory, takeExtension, (</>))
import System.FilePath qualified as FP
import System.IO (hFlush, hPutStrLn, readFile', stderr, stdout)
import System.Posix.IO (OpenMode (ReadWrite), closeFd, defaultFileFlags, dupTo, fdWrite, openFd, stdError, stdInput, stdOutput)
import System.Posix.IO.ByteString qualified as BSIO
import System.Posix.Process (executeFile, getProcessID)
import System.Posix.Terminal (TerminalMode (EnableEcho, ProcessInput), TerminalState (Immediately), getTerminalAttributes, setTerminalAttributes, withoutMode)
import System.Process.Typed hiding (setEnv)

import Fzfx.Core

dispatch :: Subcmd -> [String] -> IO ()
dispatch sub rest = cmdFor sub (map T.pack rest)

cmdFor :: Subcmd -> [Text] -> IO ()
cmdFor = \case
    SReload -> one cmdReload
    SPreview -> cmdPreview
    STransform -> one cmdTransform
    SToggle -> one cmdToggle
    SNavigate -> cmdNavigate
    SEdit -> one cmdEdit
    SMagit -> one cmdMagit
    SForgit -> one cmdForgit
    SCopy -> one cmdCopy
    SDebug -> none cmdDebug
    SSwap -> one cmdSwap
    SFullPreview -> one cmdFullPreview
    SQueryPush -> one cmdQueryPush
    SQueryPop -> none cmdQueryPop
    SQueryApply -> none cmdQueryApply
    SQueryDelete -> one cmdQueryDelete
    SQueryList -> none cmdQueryList
    SSelSave -> one cmdSelSave
    SSelRestore -> none cmdSelRestore
    SSmartEnter -> one cmdSmartEnter
    SExtraArgs -> one cmdExtraArgs
    SPreviewWidth -> none cmdPreviewWidth
    SZoxide -> one cmdZoxide
    STokei -> one cmdTokei
    SHeightToggle -> one cmdHeightToggle
  where
    one f = f . T.unwords
    none f _ = f

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

-- | Guard against removeDirectoryRecursive on a non-fzfx path.
isFzfxTmpDir :: FilePath -> Bool
isFzfxTmpDir p =
    let base = FP.takeFileName p
     in "fzfx-" `L.isPrefixOf` base && all (`elem` ("0123456789" :: String)) (drop 5 base)

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
        tyArgs = case cFd of
            FdFiles -> ["--type", "f"]
            FdDirs -> ["--type", "d"]
            FdMixed -> ["--type", "f", "--type", "d"]
        fa =
            tyArgs
                <> [ "--exclude"
                   , ".git"
                   , "--exclude"
                   , "node_modules"
                   , "--strip-cwd-prefix"
                   ]
                <> ["--hidden" | cHid]
                <> ["-L"]
                <> ["--no-ignore-vcs" | cIgn]
    out <- readProc "fd" fa
    extraFiles <- loadFzfxInclude tyArgs
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
                tr <- Set.fromList . map (pfx <>) . filter (not . T.null) . T.lines <$> readProc "git" ["ls-files", "--others", "--exclude-standard"]
                let classifyFile f =
                        let rp = pfx <> f
                         in if
                                | Set.member rp u -> Unstaged
                                | Set.member rp s -> Staged
                                | Set.member rp tr -> Untracked
                                | otherwise -> Clean
                    anyUnder st dir' =
                        let d = T.dropWhileEnd (== '/') dir'
                            dp = pfx <> d <> "/"
                         in any (dp `T.isPrefixOf`) (Set.toList st)
                    classifyDir d =
                        if
                            | anyUnder u d -> Unstaged
                            | anyUnder s d -> Staged
                            | anyUnder tr d -> Untracked
                            | otherwise -> Clean
                    classify f = case cFd of
                        FdDirs -> classifyDir f
                        FdFiles -> classifyFile f
                        FdMixed -> let fs = classifyFile f in if fs /= Clean then fs else classifyDir f
                pure [(classify f, f) | f <- files]
    let statusOk = filter (\(st, _) -> maybe True (== st) sf) labeled
        ok = if cGitSt then filter (\(st, _) -> st /= Clean) statusOk else statusOk
        (top, bot) = partition (\(_, f) -> "src/" `T.isPrefixOf` f) ok
        dotEntry
            | cFd == FdFiles = []
            | any (\(st, _) -> st == Unstaged) labeled = [(Unstaged, ".")]
            | any (\(st, _) -> st == Staged) labeled = [(Staged, ".")]
            | any (\(st, _) -> st == Untracked) labeled = [(Untracked, ".")]
            | otherwise = [(Clean, ".")]
        final = dotEntry <> top <> bot
        -- Compute positions of saved selections for restore
        savedSel = case cFd of
            FdFiles -> Set.fromList cSavedFileSel
            FdMixed -> Set.fromList cSavedFileSel
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
            FdMixed -> void $ modConfig $ \x -> x{cSavedFileSel = []}
            FdDirs -> void $ modConfig $ \x -> x{cSavedDirSel = []}
    -- Write stats for preview sizing
    let maxW = if null final then 0 else maximum [T.length f + 2 | (_, f) <- final]
    writeFile (t cDir </> "max-width") (show maxW)
    writeFile (t cDir </> "line-count") (show (length final))
    mapM_ (\(st, f) -> TIO.putStrLn (gitStatusChar st <> "\t" <> f)) final
    hFlush stdout

loadFzfxInclude :: [Text] -> IO [Text]
loadFzfxInclude tyArgs = do
    exists <- doesFileExist ".fzfxinclude"
    if not exists
        then pure []
        else do
            content <- T.pack <$> readFile' ".fzfxinclude"
            let paths = filter (\l -> not (T.null l) && not ("#" `T.isPrefixOf` l)) $ map T.strip $ T.lines content
            if null paths
                then pure []
                else do
                    let args =
                            tyArgs
                                <> ["--no-ignore", "--hidden", "-L", "--exclude", ".git"]
                                <> concatMap (\p -> ["--search-path", p]) paths
                        strip f = fromMaybe f (T.stripPrefix "./" f)
                    catch
                        (map strip . filter (not . T.null) . T.lines <$> readProc "fd" args)
                        (\(_ :: IOException) -> pure [])

reloadRgLive :: Text -> [Text] -> IO ()
reloadRgLive pat ex = unless (T.null pat) $ do
    let args =
            ["--column", "--line-number", "--no-heading", "--color=always", "--smart-case"]
                <> ex
                <> ["--", pat]
    runProcess_ $ setStdout inherit $ setStderr nullStream $ proc (t "rg") (map t args)

-- | Filter a list of lines through fzf --filter (identity when filter is empty)
fzfFilter :: Text -> [Text] -> IO [Text]
fzfFilter filt xs
    | T.null filt = pure xs
    | otherwise = do
        let input = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines xs
        (_, out, _) <- readProcess $ setStdin (byteStringInput input) $ proc "fzf" ["--filter", t filt]
        pure $ filter (not . T.null) $ T.lines $ decodeOut out

reloadRgLocked :: Text -> Text -> [Text] -> IO ()
reloadRgLocked pat filt ex = unless (T.null pat) $ do
    let rgArgs =
            ["--files-with-matches", "--no-heading", "--color=never", "--smart-case", "--sort=path"]
                <> ex
                <> ["--", pat]
    out <- catch (readProc "rg" rgArgs) (\(_ :: IOException) -> pure "")
    filtered <- fzfFilter filt $ filter (not . T.null) (T.lines out)
    mapM_ (\f -> TIO.putStrLn (" \t" <> f)) filtered
    hFlush stdout

reloadFzfRg :: Text -> Text -> [Text] -> IO ()
reloadFzfRg filt rgPat ex = unless (T.null rgPat) $ do
    fdOut <- readProc "fd" ["--type", "f", "--exclude", ".git", "--exclude", "node_modules", "--strip-cwd-prefix"]
    targets <- fzfFilter filt $ filter (not . T.null) (T.lines fdOut)
    unless (null targets) $ do
        let rgArgs =
                ["--with-filename", "--column", "--line-number", "--no-heading", "--color=always", "--smart-case"]
                    <> ex
                    <> ["--", rgPat]
                    <> targets
        runProcess_ $ setStdout inherit $ setStderr nullStream $ proc "rg" (map t rgArgs)

reloadFzfRgPending :: Text -> IO ()
reloadFzfRgPending filt = do
    fdOut <- readProc "fd" ["--type", "f", "--exclude", ".git", "--exclude", "node_modules", "--strip-cwd-prefix"]
    filtered <- fzfFilter filt $ filter (not . T.null) (T.lines fdOut)
    mapM_ (\f -> TIO.putStrLn (" \t" <> f)) filtered
    hFlush stdout

showSymlink :: Text -> IO ()
showSymlink path = do
    isSym <- pathIsSymbolicLink (t path) `catch` \(_ :: IOException) -> pure False
    when isSym $ do
        target <- T.pack <$> getSymbolicLinkTarget (t path)
        TIO.putStrLn $ "\ESC[36m→ " <> target <> "\ESC[0m"
        hFlush stdout

batHighlight :: Text -> Int -> [Text] -> IO ()
batHighlight file ln extraArgs =
    exec "bat" $ ["--color=always", "--style=numbers", "--highlight-line", showT ln] <> extraArgs <> ["--", file]

cmdPreview :: [Text] -> IO ()
cmdPreview args = withCfg $ \Config{..} -> do
    let item = fromMaybe "" (listToMaybe args)
        query = fromMaybe "" (listToMaybe (drop 1 args))
    if T.null (T.strip item)
        then case tryBookmark (stripAnsi query) of
            Just (f, ln, _) -> highlightPreview f ln
            Nothing -> contentPreview (not (T.null cGit)) "."
        else case parseFzfItem item of
            RgLine file ln _ -> highlightPreview file ln
            BookmarkLine file ln _ -> highlightPreview file ln
            FdLine st path
                | cPreview == Diff
                , st == Unstaged || st == Staged ->
                    diffPreview st path
                | otherwise -> do
                    showSymlink path
                    contentPreview (not (T.null cGit)) path
  where
    highlightPreview file ln = do
        showSymlink file
        rows <- maybe 40 readInt <$> lookupEnv "FZF_PREVIEW_LINES"
        let start = max 1 (ln - rows `div` 2)
        batHighlight file ln ["--line-range", showT start <> ":"]
    readInt s = case reads s of [(n, _)] -> n; _ -> 40 :: Int

diffArgs :: GitStatus -> Text -> [Text]
diffArgs st path = case st of
    Unstaged -> ["diff", "--", path]
    Staged -> ["diff", "--cached", "--", path]
    Untracked -> ["diff", "--no-index", "--", "/dev/null", path]
    Clean -> []

diffPreview :: GitStatus -> Text -> IO ()
diffPreview st path = do
    let da = diffArgs st path
    if null da
        then contentPreview True path
        else piped ("git", da) ("delta", [])

ezaTreeArgs :: Bool -> Text -> [Text]
ezaTreeArgs inGit p =
    ["--icons", "--git-ignore", "--tree", "-L", "3", "--color=always"]
        <> (if inGit then ["-l", "--no-permissions", "--no-filesize", "--no-user", "--no-time", "--git"] else [])
        <> [p]

contentPreview :: Bool -> Text -> IO ()
contentPreview inGit path = do
    isDir <- doesDirectoryExist (t path)
    if isDir
        then do
            p <- if path == "." then T.pack <$> getCurrentDirectory else pure path
            exec "eza" (ezaTreeArgs inGit p)
        else
            if takeExtension (t path) == ".ipynb"
                then
                    exec "nbpreview" [path]
                else
                    exec "bat" [path, "--style=plain", "--color=always", "--line-range", "0:100"]

cmdFullPreview :: Text -> IO ()
cmdFullPreview line = withCfg $ \Config{..} -> do
    tmp <- getTemporaryDirectory
    let lkFile = tmp </> "fzfx-lesskey"
    writeFile lkFile "#command\n\\e\\e quit\n\\n quit\n\\r quit\n^G quit\n\\e/ quit\n^X^S quit\n"
    case parseFzfItem line of
        RgLine file ln _ -> highlightFull lkFile file ln
        BookmarkLine file ln _ -> highlightFull lkFile file ln
        FdLine st path -> do
            let pager = "less -Rc~ --lesskey-src=" <> lkFile
            if cPreview == Diff && st /= Clean
                then do
                    let da = diffArgs st path
                    piped ("git", da) ("delta", ["--paging=always", "--pager", T.pack pager])
                else do
                    isDir <- doesDirectoryExist (t path)
                    if isDir
                        then do
                            p <- if path == "." then T.pack <$> getCurrentDirectory else pure path
                            piped ("eza", ezaTreeArgs (not (T.null cGit)) p) ("less", ["-Rc~", "--lesskey-src=" <> T.pack lkFile])
                        else exec "bat" ["--color=always", "--style=plain", "--paging=always", "--pager", T.pack pager, "--", path]
  where
    highlightFull lkFile file ln =
        let pager = "less -Rc~ -j.5 +" <> show ln <> "g --lesskey-src=" <> lkFile
         in batHighlight file ln ["--paging=always", "--pager", T.pack pager]

queryStackDir :: IO FilePath
queryStackDir = do
    home <- envOr "HOME" "/tmp"
    pure $ t home </> ".local" </> "state" </> "fzfx"

queryStackFile :: Text -> IO FilePath
queryStackFile gitRoot = do
    dir <- queryStackDir
    let name = concatMap (\c -> if c == '_' then "__" else if c == pathSeparator then "_" else [c]) (t gitRoot)
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
        TIO.putStr $ renderActions [ReloadSync (cSelf c <> " " <> flg SReload <> " " <> sel), ChangeQuery sel]
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
    (cmd, edArgs) <- getEditorCmdPlusArgs
    case parseFzfItem line of
        RgLine f ln col -> do
            posStyle <- guessEditorPosStyle cmd
            executeFile cmd True (edArgs <> editorPosArgs posStyle (t f) ln col) Nothing
        BookmarkLine f ln col -> do
            posStyle <- guessEditorPosStyle cmd
            executeFile cmd True (edArgs <> editorPosArgs posStyle (t f) ln col) Nothing
        FdLine _ p -> executeFile cmd True (edArgs <> [t p]) Nothing

getEditorCmdPlusArgs :: IO (String, [String])
getEditorCmdPlusArgs = do
    editorWords <- T.words <$> envOrM "FZFX_EDITOR" (envOr "EDITOR" "nano")
    pure $ case editorWords of
        (c : a) -> (t c, map t a)
        [] -> ("nano", [])

data EditorPosStyle
    = -- | +line:col file
      PosEmacs
    | -- | +line,col file
      PosNano
    | -- | +call cursor(line,col) file
      PosVim
    | -- | --goto file:line:col
      PosGoto
    | -- | file:line:col
      PosFileColon
    | -- | --line L --column C file
      PosFlags
    | -- | file only
      PosNone

{- | Determine position argument style from FZFX_EDITOR_HANDLE_POS_ARGS_LIKE
env var, falling back to the editor command's basename.
-}
guessEditorPosStyle :: String -> IO EditorPosStyle
guessEditorPosStyle cmd = do
    override <- lookupEnv "FZFX_EDITOR_HANDLE_POS_ARGS_LIKE"
    pure $ classify (fromMaybe (FP.takeBaseName cmd) override)
  where
    classify name
        | name `elem` emacsLike = PosEmacs
        | name `elem` nanoLike = PosNano
        | name `elem` vimLike = PosVim
        | name `elem` vscodeLike = PosGoto
        | name `elem` colonLike = PosFileColon
        | name `elem` flagLike = PosFlags
        | otherwise = PosNone
    emacsLike = ["emacs", "emacsclient", "mg", "micro", "kak", "jed", "mcedit"]
    nanoLike = ["nano", "pico"]
    vimLike = ["vim", "nvim", "vi", "view", "gvim", "mvim"]
    vscodeLike = ["code", "code-insiders", "codium", "code-oss"]
    colonLike = ["subl", "sublime_text", "hx", "zed"]
    flagLike = ["kate", "idea", "goland", "pycharm", "webstorm", "clion", "rubymine", "phpstorm", "rider"]

-- | Build position and file arguments adapted to the editor's expected syntax.
editorPosArgs :: EditorPosStyle -> String -> Int -> Int -> [String]
editorPosArgs posStyle file ln col = case posStyle of
    PosEmacs -> ["+" <> show ln <> ":" <> show col, file]
    PosNano -> ["+" <> show ln <> "," <> show col, file]
    PosVim -> ["+call cursor(" <> show ln <> "," <> show col <> ")", file]
    PosGoto -> ["--goto", file <> ":" <> show ln <> ":" <> show col]
    PosFileColon -> [file <> ":" <> show ln <> ":" <> show col]
    PosFlags -> ["--line", show ln, "--column", show col, file]
    PosNone -> [file]

{- | Restore stdin/stdout to the terminal for become: handlers.
fzfx pipes fzf's stdin (reload data) and stdout (selection capture),
but stderr is inherited as the real pty throughout fzfx → fzf → become.
Dup stderr over the piped fds so child processes get a proper terminal.
-}
reopenTty :: IO ()
reopenTty = do
    _ <- dupTo stdError stdInput
    void $ dupTo stdError stdOutput

-- | Get terminal rows via stty on /dev/tty.
getTermRows :: IO Int
getTermRows = do
    out <- readProcMaybe "stty" ["-F", "/dev/tty", "size"]
    pure $ case out of
        Just s -> case reads (T.unpack s) of [(n, _)] -> n; _ -> 40
        Nothing -> 40

{- | Query cursor row via ANSI DSR on /dev/tty.
Saves and restores full terminal attributes to avoid corrupting readline state.
-}
getCursorRow :: IO (Maybe Int)
getCursorRow =
    catch go (\(_ :: IOException) -> pure Nothing)
  where
    go = do
        fd <- openFd "/dev/tty" ReadWrite defaultFileFlags
        savedAttrs <- getTerminalAttributes fd
        let rawAttrs = withoutMode (withoutMode savedAttrs EnableEcho) ProcessInput
        bracket_
            (setTerminalAttributes fd rawAttrs Immediately)
            (setTerminalAttributes fd savedAttrs Immediately >> closeFd fd)
            ( do
                _ <- fdWrite fd "\ESC[6n"
                resp <- readUntil fd 'R' ""
                pure $ parseRow resp
            )
    readUntil fd end acc
        | length acc > 30 = pure acc
        | otherwise = do
            bs <- BSIO.fdRead fd 1
            let c = case BS.unpack bs of (x : _) -> chr (fromIntegral x); [] -> end
            if c == end then pure acc else readUntil fd end (acc ++ [c])
    parseRow s = case break (== ';') (dropWhile (\c -> c < '0' || c > '9') s) of
        (rowStr, _) -> case reads rowStr of
            [(n, _)] -> Just n
            _ -> Nothing

-- | Resolve "auto" height: space below cursor, min 50% of terminal.
resolveAutoHeight :: IO (Text, Int)
resolveAutoHeight = do
    termH <- getTermRows
    mRow <- getCursorRow
    let minH = max 10 (termH `div` 2)
        h = case mRow of
            Just row -> max minH (termH - row)
            Nothing -> minH
    pure (showT h, 0)

cmdPreviewWidth :: IO ()
cmdPreviewWidth = withCfg $ \Config{..} -> do
    let widthFile = t cDir </> "max-width"
    exists <- doesFileExist widthFile
    when exists $ do
        content <- readFile' widthFile
        let maxW = readInt 0 content
        case cPreviewLayout of
            PreviewRight -> do
                termW <- envInt "FZF_COLUMNS" =<< envInt "COLUMNS" 120
                let prevPct = max 50 (((termW - maxW - 4) * 100) `div` termW)
                TIO.putStr $ "change-preview-window(right:" <> showT prevPct <> "%)"
            PreviewBottom -> do
                lineCount <- catch (readInt 0 <$> readFile' (t cDir </> "line-count")) (\(_ :: IOException) -> pure 0)
                termH <- envInt "FZF_LINES" =<< envInt "LINES" 40
                let prevPct = max 50 (((termH - lineCount - 4) * 100) `div` termH)
                TIO.putStr $ "change-preview-window(bottom:" <> showT prevPct <> "%)"
    hFlush stdout
  where
    readInt def s = case reads s of [(n, _)] -> n; _ -> def :: Int
    envInt name def =
        lookupEnv name >>= \case
            Just s | readInt 0 s > 0 -> pure (readInt 0 s)
            _ -> pure def

cmdDebug :: IO ()
cmdDebug = withCfg $ \cfg -> do
    reopenTty
    let args = fzfArgs cfg
        self = cSelf cfg
        abbrev = "fzfx"
        content =
            T.unlines $
                toggleSummary cfg
                    <> ["", "# fzfArgs  (fzfx=" <> self <> ")", ""]
                    <> map (formatArg self abbrev) args
                    <> ["", "# config", ""]
                    <> prettyConfig cfg
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
            , "  " <> bold "M-a" <> "    @ prefix    " <> if cAt then on "ON" else off "off"
            , "  "
                <> bold "C-t"
                <> "    type         "
                <> (if cFd == FdDirs then on "dirs" else off "dirs")
                <> " / "
                <> (if cFd == FdFiles then on "files" else off "files")
                <> " / "
                <> (if cFd == FdMixed then on "mixed" else off "mixed")
            , "  " <> bold "M-m" <> "    mixed pref   " <> if cMixed then on "ON" else off "off"
            , "  " <> bold "M-h" <> "    hidden       " <> if cHid then on "ON" else off "off"
            , "  " <> bold "M-i" <> "    no-ignore    " <> if cIgn then on "ON" else off "off"
            , "  " <> bold "M-g" <> "    git status   " <> if cGitSt then on "ON" else off "off"
            , "  "
                <> bold "C-M-g"
                <> "  diff/content "
                <> (if cPreview == Diff then on "diff" else off "diff")
                <> " / "
                <> (if cPreview == Content then on "content" else off "content")
            , "  " <> bold "M-p" <> "    preview      " <> if cPreviewOn then on "ON" else off "off"
            , "  " <> bold "C-p" <> "    layout       " <> (case cPreviewLayout of PreviewRight -> on "right"; PreviewBottom -> on "bottom")
            , "  " <> bold "C-f" <> "    height       " <> if cHeightAuto then on "auto" else on "full"
            , "  " <> bold "out" <> "    output       " <> (case cOut of OTmux -> on "tmux"; OStdout -> on "stdout")
            ]
    prettifyBash s =
        L.foldl'
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
                , ("cPreview", "preview_mode", showT cPreview)
                , ("cGitSt", "git_status", showT cGitSt)
                , ("cPreviewOn", "preview_on", showT cPreviewOn)
                , ("cPreviewLy", "preview_lay", showT cPreviewLayout)
                , ("cHeight", "height", cHeight)
                , ("cHtAuto", "height_auto", showT cHeightAuto)
                , ("cMinHt", "min_height", showT cMinHeight)
                , ("cPrompt", "prompt", cPrompt)
                , ("cMixed", "mixed", showT cMixed)
                , ("cFileQ", "file_query", cFileQuery)
                , ("cDirQ", "dir_query", cDirQuery)
                , ("cWasRg", "was_rg", showT cWasRg)
                ]
            pad n s = s <> T.replicate (max 0 (n - T.length s)) " "
         in map (\(f, desc, v) -> pad 8 f <> pad 14 desc <> " = " <> v) fields

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
    exec "magit-file-status" [lineFile (parseFzfItem line)]

cmdForgit :: Text -> IO ()
cmdForgit line = withCfg $ \_ ->
    exec "git-forgit" ["log", "--", lineFile (parseFzfItem line)]

cmdTokei :: Text -> IO ()
cmdTokei line = withCfg $ \Config{..} -> do
    reopenTty
    let dir = case parseFzfItem line of
            FdLine _ p ->
                let fp = t cCwd </> t p
                 in T.pack $ takeDirectory fp
            RgLine f _ _ -> T.pack $ takeDirectory (t f)
            BookmarkLine f _ _ -> T.pack $ takeDirectory (t f)
    piped ("tokei", [dir]) ("less", ["-R"])

cmdCopy :: Text -> IO ()
cmdCopy line = do
    let path = T.stripEnd $ lineFile (parseFzfItem line)
    runProcess_ $
        setStdin (byteStringInput (LBS.fromStrict (TE.encodeUtf8 path))) $
            proc "tmux" ["load-buffer", "-w", "-"]

-- | Drop ChangePrompt actions when a custom prompt override is set
filterPrompt :: Config -> [FzfAction] -> [FzfAction]
filterPrompt cfg
    | T.null (cPrompt cfg) = id
    | otherwise = filter (\case ChangePrompt{} -> False; _ -> True)

cmdTransform :: Text -> IO ()
cmdTransform q = withCfg $ \cfg -> do
    let (cfg', actions) = transition cfg (EvTransform q)
    saveConfig cfg'
    TIO.putStr (renderActions (filterPrompt cfg' actions))
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
            "type_mixed" -> TgTypeMixed
            "mixed" -> TgMixed
            _ -> TgAtPrefix -- fallback, no-op if already matching -- TODO: wtf is this for?
        (cfg', actions) = transition cfg (EvToggle tgName curQ)
    unless (null actions) $ do
        saveConfig cfg'
        TIO.putStr (renderActions (filterPrompt cfg' actions))
        hFlush stdout

cmdNavigate :: [Text] -> IO ()
cmdNavigate args = do
    let (navAction, sel, query) = case args of
            (a : l : q : _) -> (a, l, q)
            _ -> error $ "navigate: expected 3 args, got " <> show (length args)
    Config{..} <- loadConfig
    let sp = T.dropWhileEnd (== pathSeparator) $ lineFile (parseFzfItem sel)
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
    let nFd = if navAction == "into" && cFd == FdDirs then defaultFdType else cFd
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
    -- Auto-disable git-status when navigating to a dir with no dirty files
    nGitSt <-
        if not cGitSt || T.null nGit
            then pure False
            else do
                out <- readProcMaybe "git" ["-C", nCwd', "status", "--porcelain"]
                pure $ maybe False (not . T.null . T.strip) out
    let nPrev = if cGitSt && not nGitSt then Content else cPreview
        cfg = Config{cCwd = nCwd', cQuery = nQuery, cFd = nFd, cGitSt = nGitSt, cPreview = nPrev, cFileQuery = nFileQ, cDirQuery = nDirQ, ..}
    relaunch cfg

cmdZoxide :: Text -> IO ()
cmdZoxide _query = do
    Config{..} <- loadConfig
    reopenTty
    (ec, out, _) <- readProcess $ proc "zoxide" ["query", "--interactive"]
    case ec of
        ExitSuccess -> do
            let nCwd = T.strip (decodeOut out)
            unless (T.null nCwd) $
                relaunch Config{cCwd = nCwd, cQuery = "", cFd = defaultFdType, cFileQuery = "", cDirQuery = "", cSavedFileSel = [], cSavedDirSel = [], ..}
        _ -> pure ()

{- | Save updated config and start a new fzf instance.
Callers override specific fields via record update before passing.
State is threaded entirely through the config file — _FZFX_STATE_DIR
(already set) tells mainLaunch to load from file instead of env vars.
-}
relaunch :: Config -> IO ()
relaunch cfg = do
    git <- detectGit (cCwd cfg)
    self <- T.pack <$> getExecutablePath
    saveConfig cfg{cGit = git, cSelf = self}
    mainLaunch defaultRunOpts

cmdHeightToggle :: Text -> IO ()
cmdHeightToggle query = do
    cfg@Config{..} <- loadConfig
    let toAuto = not cHeightAuto
    relaunch cfg{cQuery = query, cHeight = if toAuto then "auto" else "100%", cHeightAuto = toAuto}

detectGit :: Text -> IO Text
detectGit dir = do
    ignored <- isGitIgnored dir
    if ignored
        then pure ""
        else fromMaybe "" <$> readProcMaybe "git" ["-C", dir, "rev-parse", "--show-toplevel"]

{- | Check if dir (or any parent) is listed in ~/.fzfx-git-ignore.
File format: one path per line, blanks and #-comments skipped, paths canonicalized.
-}
isGitIgnored :: Text -> IO Bool
isGitIgnored dir = do
    home <- fromMaybe "/tmp" <$> lookupEnv "HOME"
    let ignorePath = home </> ".fzfx-git-ignore"
    exists <- doesFileExist ignorePath
    if not exists
        then pure False
        else do
            content <- readFile' ignorePath
            let paths =
                    [ l
                    | line <- lines content
                    , let l = dropWhile (== ' ') line
                    , not (null l)
                    , '#' `notElem` take 1 l
                    ]
            canonPaths <- mapM (\p -> catch (canonicalizePath p) (\(_ :: IOException) -> pure p)) paths
            canonDir <- catch (canonicalizePath (t dir)) (\(_ :: IOException) -> pure (t dir))
            let parents = iterate takeDirectory canonDir
                -- takeDirectory eventually fixpoints at "/"
                unique = takeWhile (\d -> takeDirectory d /= d) parents ++ ["/"]
            pure $ any (`elem` canonPaths) unique

-- ═══════════════════════════════════════════════════════════════════════
-- FZF Argument Building
-- ═══════════════════════════════════════════════════════════════════════
-- NOTE: fzf auto-quotes placeholder expressions ({}, {q}, {+}, etc.)
-- for safe shell passing. Do NOT manually quote them — that causes
-- double-quoting. See `man fzf` under "placeholder".

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
    let mode = case cFd of FdFiles -> "file"; FdDirs -> "dir"; FdMixed -> "file"
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
        , "--prompt=" <> if T.null cPrompt then (case cFd of FdDirs -> "dirs"; FdMixed -> "mixed"; FdFiles -> "files") <> "> " else cPrompt
        , "--height=" <> cHeight
        ]
            <> ["--min-height=" <> showT cMinHeight | cMinHeight > 0]
            <> [ "--query=" <> cQuery
               , "--preview=" <> cSelf <> " " <> flg SPreview <> " {} {q}"
               , "--preview-window="
                    <> (case cPreviewLayout of PreviewRight -> "right"; PreviewBottom -> "bottom")
                    <> ":50%"
                    <> (if cPreviewOn then "" else ":hidden")
               ]
    selfBindings =
        [ xf cfg "change" STransform "{q}"
        , xf cfg "alt-@" SToggle "at_prefix"
        , xe cfg "alt-/" SFullPreview "{}" ""
        , xf cfg "alt-g" SToggle "git_status"
        , bind "alt-u" (statusToggle "U")
        , bind "alt-s" (statusToggle "S")
        , bind "alt-?" (statusToggle "?")
        , xf cfg "alt-h" SToggle "hidden"
        , xf cfg "alt-i" SToggle "no_ignore"
        , xf cfg "alt-m" SToggle "mixed"
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
        , bc cfg "ctrl-f" SHeightToggle "{q}"
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
        , bind "ctrl-alt-r" ("reload-sync(" <> cSelf <> " " <> flg SReload <> " {q})")
        , bind "ctrl-h" ("execute(" <> cSelf <> " --help | less -R)")
        , bind "ctrl-g" "abort"
        , bind "ctrl-z" "abort"
        , xf cfg "f4" SQueryPush "{q}"
        , xe cfg "f3" SQueryPop "" ("+transform:" <> cSelf <> " " <> flg SQueryApply)
        , bind "zero" ("preview(" <> cSelf <> " " <> flg SPreview <> " '' {q})")
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
            | isFzfLine line = case parseFzfItem line of
                RgLine f ln col -> makeRelPath cOrig cCwd f <> ":" <> showT ln <> ":" <> showT col
                BookmarkLine f ln col -> makeRelPath cOrig cCwd f <> ":" <> showT ln <> ":" <> showT col
                FdLine _ p -> makeRelPath cOrig cCwd p
            | otherwise = case tryRg (stripAnsi line) of
                Just (f, ln, col) -> makeRelPath cOrig cCwd f <> ":" <> showT ln <> ":" <> showT col
                Nothing -> line
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
    , optHeight :: !(Maybe Text)
    , optPrompt :: !(Maybe Text)
    , optPane :: !(Maybe Text)
    , optQuery :: ![Text]
    }

-- | All-default opts for internal re-launch (state comes from config file)
defaultRunOpts :: RunOpts
defaultRunOpts =
    RunOpts
        { optCwd = Nothing
        , optOutput = Nothing
        , optType = Nothing
        , optHidden = False
        , optAtPrefix = False
        , optGitStatus = False
        , optPreview = Nothing
        , optPreviewLayout = Nothing
        , optHeight = Nothing
        , optPrompt = Nothing
        , optPane = Nothing
        , optQuery = []
        }

parseOutMode :: ReadM OutMode
parseOutMode = eitherReader $ \case
    "stdout" -> Right OStdout
    "tmux" -> Right OTmux
    s -> Left $ "unknown output mode: " <> s <> " (expected stdout|tmux)"

parsePreviewLayout :: ReadM PreviewLayout
parsePreviewLayout = eitherReader $ \case
    "right" -> Right PreviewRight
    "bottom" -> Right PreviewBottom
    s -> Left $ "unknown layout: " <> s <> " (expected right|bottom)"

parseFdType :: ReadM FdType
parseFdType = eitherReader $ \case
    "f" -> Right FdFiles
    "d" -> Right FdDirs
    "m" -> Right FdMixed
    "files" -> Right FdFiles
    "dirs" -> Right FdDirs
    "mixed" -> Right FdMixed
    s -> Left $ "unknown type: " <> s <> " (expected f|d|m|files|dirs|mixed)"

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
                    <> help "File type filter (f|d|m|files|dirs|mixed)"
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
                    ( long "height"
                        <> metavar "HEIGHT"
                        <> help "FZF height (e.g. 40%, 100%, auto)"
                    )
            )
        <*> optional
            ( T.pack
                <$> strOption
                    ( long "prompt"
                        <> metavar "PROMPT"
                        <> help "Override fzf prompt string"
                    )
            )
        <*> optional
            ( T.pack
                <$> strOption
                    ( long "tmux-target-pane"
                        <> metavar "PANE"
                        <> help "Tmux pane to send results to (e.g. %1). Required with --output tmux when called from outside a tmux session"
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
        , "  alt-enter       open in editor ($EDITOR)"
        , "  tab             toggle selection down"
        , "  shift-tab       toggle + clear line"
        , "  alt-3           switch to ripgrep mode"
        , "  alt-@           toggle @ prefix on output"
        , "  alt-r           swap query/results"
        , "  alt-/           full-screen preview in less"
        , "  alt-g           toggle git status filter"
        , "  ctrl-alt-g      toggle diff preview"
        , "  alt-m           toggle mixed mode (files+dirs)"
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
        , "  ctrl-t          toggle files/dirs/mixed"
        , "  alt-u/s/?       filter by git status"
        , "  ctrl-p          toggle preview layout (right/bottom)"
        , "  alt-p           toggle preview"
        , "  alt-;           cycle preview layout"
        , "  alt-{/}         preview half-page up/down"
        , "  alt-space       preview page down"
        , "  ctrl-space      preview page up"
        , "  ctrl-f          toggle height (full/auto)"
        , "  ctrl-alt-r      reload"
        , "  ctrl-h          show help in pager"
        , "  ctrl-alt-d      debug info"
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
        , "Config:"
        , "  ~/.fzfx-git-ignore   skip git ops for listed repo paths"
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
mainLaunch opts =
    lookupEnv "_FZFX_STATE_DIR" >>= \case
        Just sd | not (null sd) -> do
            -- Relaunch: config was saved by `relaunch`, load and run
            cfg <- loadConfig
            setCurrentDirectory (t (cCwd cfg))
            -- Resolve "auto" height (needs terminal dimensions at launch time)
            cfg' <-
                if cHeightAuto cfg
                    then do
                        (h, mh) <- resolveAutoHeight
                        pure cfg{cHeight = h, cMinHeight = mh}
                    else pure cfg
            launchFzf cfg'
        _ -> firstLaunch opts

firstLaunch :: RunOpts -> IO ()
firstLaunch opts = do
    cfg <- buildConfig opts
    setCurrentDirectory (t (cCwd cfg))
    let sd = cDir cfg
    bracket
        (saveConfig cfg >> pure sd)
        ( \d ->
            when (isFzfxTmpDir (t d)) $
                catch (removeDirectoryRecursive (t d)) (\(_ :: IOException) -> pure ())
        )
        $ \_ -> do
            setEnv "_FZFX_STATE_DIR" (t sd)
            launchFzf cfg

buildConfig :: RunOpts -> IO Config
buildConfig RunOpts{..} = do
    self <- T.pack <$> getExecutablePath
    cwd <- case optCwd of
        Just d -> pure d
        Nothing -> T.pack <$> getCurrentDirectory
    (pane, paneTty) <- resolveTmuxPaneAndTty optPane
    let om = fromMaybe OStdout optOutput
    when (om == OTmux && T.null pane) $ do
        hPutStrLn stderr "fzfx: --output tmux requires --tmux-target-pane or a tmux session"
        exitFailure
    at <-
        if optAtPrefix
            then pure True
            else
                if om == OTmux && not (T.null pane)
                    then catch (detectAi paneTty) (\(_ :: IOException) -> pure False)
                    else pure False
    let fd = fromMaybe defaultFdType optType
        gitSt = optGitStatus
        heightRaw = fromMaybe "100%" optHeight
        heightAuto = heightRaw == "auto"
        q = T.unwords optQuery
    (height, minHeight) <-
        if heightAuto
            then resolveAutoHeight
            else pure (heightRaw, 0)
    git <- detectGit cwd
    tmp <- getTemporaryDirectory
    pid <- getProcessID
    persistedStack <- loadQueryStack git
    pure
        Config
            { cDir = T.pack $ tmp </> "fzfx-" <> show pid
            , cGit = git
            , cOrig = cwd
            , cCwd = cwd
            , cPane = pane
            , cSelf = self
            , cQuery = q
            , cOut = om
            , cAt = at
            , cFd = fd
            , cHid = optHidden
            , cIgn = False
            , cPreview = if gitSt then Diff else Content
            , cFileQuery = if fd `elem` [FdFiles, FdMixed] then q else ""
            , cDirQuery = if fd == FdDirs then q else ""
            , cQueryStack = persistedStack
            , cPendingQuery = ""
            , cWasRg = False
            , cSavedFileSel = []
            , cSavedDirSel = []
            , cGitSt = gitSt
            , cPreviewOn = fromMaybe True optPreview
            , cPreviewLayout = fromMaybe PreviewRight optPreviewLayout
            , cHeight = height
            , cHeightAuto = heightAuto
            , cMinHeight = minHeight
            , cPrompt = fromMaybe "" optPrompt
            , cMixed = fd == FdMixed
            }

-- | Pipe reload into fzf, wait for selection, output results.
launchFzf :: Config -> IO ()
launchFzf cfg = do
    let self = cSelf cfg
        q = cQuery cfg
        reloadProc =
            setStdout createPipe $
                proc (t self) [t (flg SReload), t q]
        fzfCmd = map t (fzfArgs cfg)
    -- reload feeds fzf's stdin; when fzf exits (abort, become:, selection)
    -- the pipe breaks and reload gets SIGPIPE — ignore its exit code.
    withProcessWait reloadProc $ \reloadP -> do
        let fzfProc =
                setStdin (useHandleOpen (getStdout reloadP)) $
                    setStdout createPipe $
                        proc (t "fzf") fzfCmd
        withProcessWait fzfProc $ \p -> do
            out <- LBS.hGetContents (getStdout p)
            fzfEc <- waitExitCode p
            void $ waitExitCode reloadP
            case fzfEc of
                ExitSuccess -> do
                    let selected = filter (not . T.null) (T.lines (decodeOut out))
                    unless (null selected) $ do
                        c <- loadConfig
                        outputResults c selected
                _ -> pure ()

{- | Resolve tmux pane id and tty. Fetches both in a single tmux call
when auto-detecting; uses a targeted query for explicit pane ids.
-}
resolveTmuxPaneAndTty :: Maybe Text -> IO (Text, Maybe Text)
resolveTmuxPaneAndTty optPane = do
    inTmux <- maybe False (not . null) <$> lookupEnv "TMUX"
    case optPane of
        Just p -> do
            tty <- if inTmux then readProcMaybe "tmux" ["display-message", "-t", p, "-p", "#{pane_tty}"] else pure Nothing
            pure (p, tty)
        Nothing ->
            if not inTmux
                then pure ("", Nothing)
                else do
                    out <- readProcMaybe "tmux" ["display-message", "-p", "#{pane_id}\t#{pane_tty}"]
                    pure $ case out of
                        Just txt
                            | (pid', tty) <- T.breakOn "\t" txt
                            , not (T.null pid') ->
                                (pid', if T.length tty > 1 then Just (T.drop 1 tty) else Nothing)
                        _ -> ("", Nothing)

-- | Check if an AI agent is running in the given tty.
detectAi :: Maybe Text -> IO Bool
detectAi mTty = case mTty of
    Nothing -> pure False
    Just tty' -> do
        (ec, out, _) <- readProcess $ proc "ps" ["-t", t tty', "-o", "comm=,args="]
        let keywords = ["claude", "codex", "gemini"]
        pure $ case ec of
            ExitSuccess ->
                any
                    (\p -> any (`T.isInfixOf` p) keywords)
                    (filter (not . T.null) (T.lines (decodeOut out)))
            _ -> False
