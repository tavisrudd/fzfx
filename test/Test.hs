{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (exitFailure, exitSuccess)

import Fzfx.Core

-- ═══════════════════════════════════════════════════════════════════════
-- Test Runner
-- ═══════════════════════════════════════════════════════════════════════

data TestResult = Pass | Fail String

test :: String -> Bool -> TestResult
test _ True = Pass
test name False = Fail name

runTests :: [TestResult] -> IO ()
runTests results = do
    let failures = [msg | Fail msg <- results]
        total = length results
        failed = length failures
    mapM_ (\msg -> putStrLn $ "  FAIL: " <> msg) failures
    putStrLn $ show (total - failed) <> "/" <> show total <> " passed"
    if null failures then exitSuccess else exitFailure

-- ═══════════════════════════════════════════════════════════════════════
-- Tests
-- ═══════════════════════════════════════════════════════════════════════

main :: IO ()
main =
    runTests $
        concat
            [ tryRgTests
            , stripAnsiTests
            , parseLineTests
            , parseQueryTests
            , parseSFilterTests
            , lineAccessorTests
            , makeRelPathTests
            , interleaveTests
            , ordNubTests
            , gitStatusCharTests
            , subcmdRoundtripTests
            , configRoundtripTests
            , showTTests
            , transitionToggleTests
            , transitionTransformTests
            , transitionSwapTests
            , transitionExtraArgsTests
            , transitionSmartEnterTests
            , transitionQueryTests
            , renderActionsTests
            ]

-- ═══════════════════════════════════════════════════════════════════════
-- tryRg
-- ═══════════════════════════════════════════════════════════════════════

tryRgTests :: [TestResult]
tryRgTests =
    [ test "tryRg basic" $
        tryRg "src/Main.hs:42:10:some code" == Just ("src/Main.hs", 42, 10)
    , test "tryRg with colons in text" $
        tryRg "file.hs:17:5:key: value" == Just ("file.hs", 17, 5)
    , test "tryRg with multiple colons in text" $
        tryRg "file.hs:1:1:a:b:c" == Just ("file.hs", 1, 1)
    , test "tryRg no match on plain filename" $
        isNothing (tryRg "CLAUDE.md")
    , test "tryRg no match on fd line" $
        isNothing (tryRg " \tCLAUDE.md")
    , test "tryRg empty" $
        isNothing (tryRg "")
    , test "tryRg only colons" $
        isNothing (tryRg ":::")
    , test "tryRg non-digit line number" $
        isNothing (tryRg "file:abc:1:text")
    , test "tryRg non-digit col" $
        isNothing (tryRg "file:1:abc:text")
    , test "tryRg trailing colon (empty text)" $
        tryRg "file:1:1:" == Just ("file", 1, 1)
    , test "tryRg path with dots" $
        tryRg "./src/Foo.hs:10:3:code" == Just ("./src/Foo.hs", 10, 3)
    , test "tryRg large line number" $
        tryRg "f:99999:1:x" == Just ("f", 99999, 1)
    , test "tryRg single char file" $
        tryRg "f:1:1:x" == Just ("f", 1, 1)
    , test "tryRg missing text after col" $
        isNothing (tryRg "f:1:1")
    , test "tryRg no match on line:col:text without filename" $
        isNothing (tryRg "37:5:    tryRg,")
    , test "tryRg no match on line:col:text with non-digit col-position" $
        isNothing (tryRg "4:29:Domain.Types: foo")
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- stripAnsi
-- ═══════════════════════════════════════════════════════════════════════

stripAnsiTests :: [TestResult]
stripAnsiTests =
    [ test "stripAnsi plain" $
        stripAnsi "hello" == "hello"
    , test "stripAnsi with color" $
        stripAnsi "\ESC[31mred\ESC[0m" == "red"
    , test "stripAnsi with bold" $
        stripAnsi "\ESC[1;33mfoo\ESC[0m:bar" == "foo:bar"
    , test "stripAnsi empty" $
        stripAnsi "" == ""
    , test "stripAnsi nested colors" $
        stripAnsi "\ESC[1m\ESC[35mfile\ESC[0m:\ESC[32m42\ESC[0m" == "file:42"
    , test "stripAnsi no-op on clean text" $
        stripAnsi "src/Main.hs:42:10:code" == "src/Main.hs:42:10:code"
    , test "stripAnsi idempotent" $
        let s = "\ESC[31mred\ESC[0m"
         in stripAnsi (stripAnsi s) == stripAnsi s
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- parseLine
-- ═══════════════════════════════════════════════════════════════════════

parseLineTests :: [TestResult]
parseLineTests =
    [ test "parseLine rg line" $
        parseLine "src/Main.hs:42:10:code here" == RgLine "src/Main.hs" 42 10
    , test "parseLine rg with ANSI" $
        parseLine "\ESC[35msrc/Main.hs\ESC[0m:42:10:code" == RgLine "src/Main.hs" 42 10
    , test "parseLine rg with colons in text" $
        parseLine "file.hs:17:5:key: value" == RgLine "file.hs" 17 5
    , test "parseLine fd clean" $
        parseLine " \tCLAUDE.md" == FdLine Clean "CLAUDE.md"
    , test "parseLine fd unstaged" $
        parseLine "U\tfile.txt" == FdLine Unstaged "file.txt"
    , test "parseLine fd staged" $
        parseLine "S\tfile.txt" == FdLine Staged "file.txt"
    , test "parseLine fd untracked" $
        parseLine "?\tfile.txt" == FdLine Untracked "file.txt"
    , test "parseLine plain filename" $
        parseLine "CLAUDE.md" == FdLine Clean "CLAUDE.md"
    , test "parseLine fd with path" $
        parseLine " \tsrc/Foo/Bar.hs" == FdLine Clean "src/Foo/Bar.hs"
    , test "parseLine fd unknown status label" $
        parseLine "X\tfile.txt" == FdLine Clean "file.txt"
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- parseQuery
-- ═══════════════════════════════════════════════════════════════════════

parseQueryTests :: [TestResult]
parseQueryTests =
    [ test "parseQuery plain text" $
        parseQuery "hello" == FileMode
    , test "parseQuery empty" $
        parseQuery "" == FileMode
    , test "parseQuery escaped hash" $
        parseQuery "\\#hello" == FileMode
    , test "parseQuery rg live" $
        parseQuery "#pattern" == RgLive "pattern" []
    , test "parseQuery rg live empty pattern" $
        parseQuery "#" == RgLive "" []
    , test "parseQuery rg locked" $
        parseQuery "#pattern#filter" == RgLocked "pattern" "filter" []
    , test "parseQuery rg locked empty filter" $
        parseQuery "#pattern#" == RgLocked "pattern" "" []
    , test "parseQuery rg with extra args" $
        parseQuery "#pat -- -g *.hs" == RgLive "pat" ["-g", "*.hs"]
    , test "parseQuery rg locked with extra args" $
        parseQuery "#pat -- -g *.hs#filt" == RgLocked "pat" "filt" ["-g", "*.hs"]
    , test "parseQuery fzfRg" $
        parseQuery "nix#import" == FzfRg "nix" "import" []
    , test "parseQuery fzfRg with extra args" $
        parseQuery "nix#pat -- -g *.hs" == FzfRg "nix" "pat" ["-g", "*.hs"]
    , test "parseQuery fzfRg trailing # pending" $
        parseQuery "nix#" == FzfRgPending "nix"
    , test "parseQuery fzfRg trailing # space pending" $
        parseQuery "nix# " == FzfRgPending "nix"
    , test "parseQuery fzfRg with spaces in filter" $
        parseQuery "foo bar#pat" == FzfRg "foo bar" "pat" []
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- parseSFilter
-- ═══════════════════════════════════════════════════════════════════════

parseSFilterTests :: [TestResult]
parseSFilterTests =
    [ test "parseSFilter no prefix" $
        parseSFilter "hello" == (Nothing, "hello")
    , test "parseSFilter empty" $
        parseSFilter "" == (Nothing, "")
    , test "parseSFilter unstaged" $
        parseSFilter "^U some query" == (Just Unstaged, "some query")
    , test "parseSFilter staged" $
        parseSFilter "^S file.hs" == (Just Staged, "file.hs")
    , test "parseSFilter untracked" $
        parseSFilter "^? new" == (Just Untracked, "new")
    , test "parseSFilter no space after prefix" $
        parseSFilter "^Unospaced" == (Nothing, "^Unospaced")
    , test "parseSFilter just prefix" $
        parseSFilter "^U " == (Just Unstaged, "")
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- lineFile / lineRef
-- ═══════════════════════════════════════════════════════════════════════

lineAccessorTests :: [TestResult]
lineAccessorTests =
    [ test "lineFile RgLine" $
        lineFile (RgLine "src/Main.hs" 42 10) == "src/Main.hs"
    , test "lineFile FdLine" $
        lineFile (FdLine Clean "README.md") == "README.md"
    , test "lineRef RgLine" $
        lineRef (RgLine "src/Main.hs" 42 10) == "src/Main.hs:42:10"
    , test "lineRef FdLine" $
        lineRef (FdLine Unstaged "file.txt") == "file.txt"
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- makeRelPath
-- ═══════════════════════════════════════════════════════════════════════

makeRelPathTests :: [TestResult]
makeRelPathTests =
    [ test "makeRelPath same dir" $
        makeRelPath "/home/user/proj" "/home/user/proj" "src/Main.hs"
            == "src/Main.hs"
    , test "makeRelPath absolute path passthrough" $
        makeRelPath "/home/user/proj" "/home/user/proj/sub" "/tmp/file"
            == "/tmp/file"
    , test "makeRelPath one level up" $
        makeRelPath "/home/user/proj" "/home/user/proj/sub" "file.hs"
            == "sub/file.hs"
    , test "makeRelPath two levels up" $
        makeRelPath "/home/user/proj" "/home/user/proj/a/b" "file.hs"
            == "a/b/file.hs"
    , test "makeRelPath sibling dir (1 up)" $
        makeRelPath "/home/user/proj/a" "/home/user/proj/b" "file.hs"
            == "../b/file.hs"
    , test "makeRelPath too many levels up (>2) falls back to absolute" $
        let result = makeRelPath "/a/b/c/d" "/a/x" "f"
         in T.isPrefixOf "/" result -- should be absolute
    , test "makeRelPath trailing sep normalization" $
        makeRelPath "/home/user/proj/" "/home/user/proj/" "file.hs"
            == "file.hs"
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- interleave
-- ═══════════════════════════════════════════════════════════════════════

interleaveTests :: [TestResult]
interleaveTests =
    [ test "interleave empty extras" $
        interleave ["a", "c", "e"] [] == ["a", "c", "e"]
    , test "interleave empty main" $
        interleave [] ["b", "d"] == ["b", "d"]
    , test "interleave both empty" $
        null (interleave [] ([] :: [Text]))
    , test "interleave merges sorted" $
        interleave ["a", "c", "e"] ["b", "d"] == ["a", "b", "c", "d", "e"]
    , test "interleave extras before first" $
        interleave ["c", "d"] ["a", "b"] == ["a", "b", "c", "d"]
    , test "interleave extras after last" $
        interleave ["a", "b"] ["c", "d"] == ["a", "b", "c", "d"]
    , test "interleave preserves all elements" $
        let m = ["b", "d", "f"]; e = ["a", "c", "e"]
         in length (interleave m e) == length m + length e
    , test "interleave duplicates" $
        interleave ["a", "c"] ["a", "b"] == ["a", "a", "b", "c"]
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- ordNub
-- ═══════════════════════════════════════════════════════════════════════

ordNubTests :: [TestResult]
ordNubTests =
    [ test "ordNub empty" $
        null (ordNub [] :: [Text])
    , test "ordNub no dupes" $
        ordNub ["a", "b", "c"] == ["a", "b", "c"]
    , test "ordNub removes dupes" $
        ordNub ["a", "b", "a", "c", "b"] == ["a", "b", "c"]
    , test "ordNub preserves first occurrence order" $
        ordNub ["c", "a", "b", "a", "c"] == ["c", "a", "b"]
    , test "ordNub all same" $
        ordNub ["x", "x", "x"] == ["x"]
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- gitStatusChar
-- ═══════════════════════════════════════════════════════════════════════

gitStatusCharTests :: [TestResult]
gitStatusCharTests =
    [ test "gitStatusChar Unstaged" $
        gitStatusChar Unstaged == "U"
    , test "gitStatusChar Staged" $
        gitStatusChar Staged == "S"
    , test "gitStatusChar Untracked" $
        gitStatusChar Untracked == "?"
    , test "gitStatusChar Clean" $
        gitStatusChar Clean == " "
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- Subcmd roundtrip (flg → parseSubcmd)
-- ═══════════════════════════════════════════════════════════════════════

subcmdRoundtripTests :: [TestResult]
subcmdRoundtripTests =
    [ test ("subcmd roundtrip " <> show sub) $
        parseSubcmd (T.unpack (flg sub)) == Just sub
    | sub <- [minBound .. maxBound]
    ]
        <> [ test "parseSubcmd unknown flag" $
                isNothing (parseSubcmd "--nonexistent")
           , test "parseSubcmd empty" $
                isNothing (parseSubcmd "")
           ]

-- ═══════════════════════════════════════════════════════════════════════
-- Config Read/Show roundtrip
-- ═══════════════════════════════════════════════════════════════════════

configRoundtripTests :: [TestResult]
configRoundtripTests =
    [ test "Config Read/Show roundtrip" $
        let cfg =
                Config
                    { cDir = "/tmp/fzfx-test"
                    , cGit = "/home/user/proj"
                    , cOrig = "/home/user/proj"
                    , cCwd = "/home/user/proj/sub"
                    , cPane = "%42"
                    , cSelf = "/nix/store/xxx/bin/fzfx"
                    , cQuery = "#pattern"
                    , cOut = OTmux
                    , cAt = True
                    , cFd = FdFiles
                    , cHid = False
                    , cIgn = True
                    , cPreview = Diff
                    , cFileQuery = "some query"
                    , cDirQuery = ""
                    , cQueryStack = ["#foo", "bar#baz"]
                    , cPendingQuery = ""
                    , cWasRg = True
                    , cSavedFileSel = ["/home/user/proj/a.hs"]
                    , cSavedDirSel = []
                    , cGitSt = False
                    , cPreviewOn = True
                    , cPreviewLayout = PreviewRight
                    , cHeight = "100%"
                    , cHeightAuto = False
                    , cMinHeight = 0
                    , cPrompt = ""
                    , cMixed = False
                    }
         in read (show cfg) == cfg
    , test "Config Read/Show roundtrip default-like" $
        let cfg =
                Config
                    { cDir = "/tmp/d"
                    , cGit = ""
                    , cOrig = "/home/user"
                    , cCwd = "/home/user"
                    , cPane = ""
                    , cSelf = "/usr/bin/fzfx"
                    , cQuery = ""
                    , cOut = OStdout
                    , cAt = False
                    , cFd = FdDirs
                    , cHid = True
                    , cIgn = False
                    , cPreview = Content
                    , cFileQuery = ""
                    , cDirQuery = ""
                    , cQueryStack = []
                    , cPendingQuery = ""
                    , cWasRg = False
                    , cSavedFileSel = []
                    , cSavedDirSel = []
                    , cGitSt = False
                    , cPreviewOn = True
                    , cPreviewLayout = PreviewRight
                    , cHeight = "40%"
                    , cHeightAuto = False
                    , cMinHeight = 0
                    , cPrompt = ""
                    , cMixed = False
                    }
         in read (show cfg) == cfg
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- showT
-- ═══════════════════════════════════════════════════════════════════════

showTTests :: [TestResult]
showTTests =
    [ test "showT Int" $
        showT (42 :: Int) == "42"
    , test "showT Bool" $
        showT True == "True"
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- Test Config Helper
-- ═══════════════════════════════════════════════════════════════════════

testCfg :: Config
testCfg =
    Config
        { cDir = "/tmp/fzfx-test"
        , cGit = "/home/user/proj"
        , cOrig = "/home/user/proj"
        , cCwd = "/home/user/proj"
        , cPane = "%42"
        , cSelf = "/usr/bin/fzfx"
        , cQuery = ""
        , cOut = OTmux
        , cAt = False
        , cFd = FdFiles
        , cHid = False
        , cIgn = False
        , cPreview = Content
        , cFileQuery = ""
        , cDirQuery = ""
        , cQueryStack = []
        , cPendingQuery = ""
        , cWasRg = False
        , cSavedFileSel = []
        , cSavedDirSel = []
        , cGitSt = False
        , cPreviewOn = True
        , cPreviewLayout = PreviewRight
        , cHeight = "100%"
        , cHeightAuto = False
        , cMinHeight = 0
        , cPrompt = ""
        , cMixed = False
        }

hasAction :: FzfAction -> [FzfAction] -> Bool
hasAction = elem

noAction :: FzfAction -> [FzfAction] -> Bool
noAction a = not . hasAction a

hasReload :: [FzfAction] -> Bool
hasReload = any (\case ReloadSync{} -> True; _ -> False)

hasFooter :: [FzfAction] -> Bool
hasFooter = any (\case ChangeFooter{} -> True; _ -> False)

hasPreviewWindow :: [FzfAction] -> Bool
hasPreviewWindow = any (\case ChangePreviewWindow{} -> True; _ -> False)

-- ═══════════════════════════════════════════════════════════════════════
-- Transition: Toggles
-- ═══════════════════════════════════════════════════════════════════════

transitionToggleTests :: [TestResult]
transitionToggleTests =
    -- @ prefix toggle
    [ test "toggle at_prefix: flips cAt" $
        let (cfg', _) = transition testCfg (EvToggle TgAtPrefix "")
         in cAt cfg'
    , test "toggle at_prefix: flips back" $
        let cfg1 = testCfg{cAt = True}
            (cfg', _) = transition cfg1 (EvToggle TgAtPrefix "")
         in not (cAt cfg')
    , test "toggle at_prefix: emits header, no reload" $
        let (_, acts) = transition testCfg (EvToggle TgAtPrefix "")
         in hasFooter acts && not (hasReload acts)
    , -- diff toggle
      test "toggle diff: Content → Diff" $
        let (cfg', _) = transition testCfg (EvToggle TgDiff "")
         in cPreview cfg' == Diff
    , test "toggle diff: Diff → Content" $
        let cfg1 = testCfg{cPreview = Diff}
            (cfg', _) = transition cfg1 (EvToggle TgDiff "")
         in cPreview cfg' == Content
    , test "toggle diff: emits refresh-preview" $
        let (_, acts) = transition testCfg (EvToggle TgDiff "")
         in hasAction RefreshPreview acts
    , -- hidden toggle
      test "toggle hidden: flips cHid" $
        let (cfg', _) = transition testCfg (EvToggle TgHidden "")
         in cHid cfg'
    , test "toggle hidden: emits reload + header" $
        let (_, acts) = transition testCfg (EvToggle TgHidden "")
         in hasReload acts && hasFooter acts
    , -- no-ignore toggle
      test "toggle no_ignore: flips cIgn" $
        let (cfg', _) = transition testCfg (EvToggle TgNoIgnore "")
         in cIgn cfg'
    , test "toggle no_ignore: emits reload + header" $
        let (_, acts) = transition testCfg (EvToggle TgNoIgnore "")
         in hasReload acts && hasFooter acts
    , -- type toggle: files → dirs
      test "toggle type: files → dirs" $
        let (cfg', _) = transition testCfg (EvToggle TgType "curquery")
         in cFd cfg' == FdDirs
    , test "toggle type: files → dirs saves file query" $
        let (cfg', _) = transition testCfg (EvToggle TgType "curquery")
         in cFileQuery cfg' == "curquery"
    , test "toggle type: files → dirs emits dirs prompt" $
        let (_, acts) = transition testCfg (EvToggle TgType "curquery")
         in hasAction (ChangePrompt "dirs> ") acts
    , test "toggle type: files → dirs restores dir query" $
        let cfg1 = testCfg{cDirQuery = "saved_dir_q"}
            (_, acts) = transition cfg1 (EvToggle TgType "curquery")
         in hasAction (ChangeQuery "saved_dir_q") acts
    , -- type toggle: dirs → files
      test "toggle type: dirs → files" $
        let cfg1 = testCfg{cFd = FdDirs}
            (cfg', _) = transition cfg1 (EvToggle TgType "dirquery")
         in cFd cfg' == FdFiles
    , test "toggle type: dirs → files saves dir query" $
        let cfg1 = testCfg{cFd = FdDirs}
            (cfg', _) = transition cfg1 (EvToggle TgType "dirquery")
         in cDirQuery cfg' == "dirquery"
    , test "toggle type: dirs → files emits files prompt" $
        let cfg1 = testCfg{cFd = FdDirs}
            (_, acts) = transition cfg1 (EvToggle TgType "dirquery")
         in hasAction (ChangePrompt "files> ") acts
    , -- type toggle: empty restore query → jump first
      test "toggle type: empty restore query → first" $
        let (_, acts) = transition testCfg (EvToggle TgType "q")
         in hasAction JumpFirst acts
    , test "toggle type: non-empty restore query → no first" $
        let cfg1 = testCfg{cDirQuery = "saved"}
            (_, acts) = transition cfg1 (EvToggle TgType "q")
         in noAction JumpFirst acts
    , -- type_d when already dirs → no-op
      test "toggle type_d when already dirs: no-op" $
        let cfg1 = testCfg{cFd = FdDirs}
            (cfg', acts) = transition cfg1 (EvToggle TgTypeD "q")
         in null acts && cFd cfg' == FdDirs
    , -- git status toggle
      test "toggle git_status: off → on, sets cGitSt" $
        let (cfg', _) = transition testCfg (EvToggle TgGitStatus "")
         in cGitSt cfg'
    , test "toggle git_status: on → off, clears cGitSt" $
        let cfg1 = testCfg{cGitSt = True, cPreview = Diff}
            (cfg', _) = transition cfg1 (EvToggle TgGitStatus "")
         in not (cGitSt cfg')
    , test "toggle git_status: on → also enables diff preview" $
        let (cfg', _) = transition testCfg (EvToggle TgGitStatus "")
         in cPreview cfg' == Diff
    , test "toggle git_status: off → also disables diff preview" $
        let cfg1 = testCfg{cGitSt = True, cPreview = Diff}
            (cfg', _) = transition cfg1 (EvToggle TgGitStatus "")
         in cPreview cfg' == Content
    , test "toggle git_status: emits reload + refresh-preview + footer" $
        let (_, acts) = transition testCfg (EvToggle TgGitStatus "")
         in hasReload acts && hasAction RefreshPreview acts && hasFooter acts
    , -- preview layout toggle
      test "toggle preview_layout: right → bottom" $
        let (cfg', _) = transition testCfg (EvToggle TgPreviewLayout "")
         in cPreviewLayout cfg' == PreviewBottom
    , test "toggle preview_layout: bottom → right" $
        let cfg1 = testCfg{cPreviewLayout = PreviewBottom}
            (cfg', _) = transition cfg1 (EvToggle TgPreviewLayout "")
         in cPreviewLayout cfg' == PreviewRight
    , test "toggle preview_layout: emits change-preview-window + reload + footer" $
        let (_, acts) = transition testCfg (EvToggle TgPreviewLayout "")
         in hasPreviewWindow acts && hasReload acts && hasFooter acts
    , test "toggle preview_layout: right → emits bottom direction" $
        let (_, acts) = transition testCfg (EvToggle TgPreviewLayout "")
         in hasAction (ChangePreviewWindow "bottom:50%") acts
    , test "toggle preview_layout: bottom → emits right direction" $
        let cfg1 = testCfg{cPreviewLayout = PreviewBottom}
            (_, acts) = transition cfg1 (EvToggle TgPreviewLayout "")
         in hasAction (ChangePreviewWindow "right:50%") acts
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- Transition: Transform
-- ═══════════════════════════════════════════════════════════════════════

transitionTransformTests :: [TestResult]
transitionTransformTests =
    [ test "transform: plain query → FileMode, files prompt" $
        let (_, acts) = transition testCfg (EvTransform "hello")
         in hasAction (ChangePrompt "files> ") acts && hasAction EnableSearch acts
    , test "transform: plain query, not wasRg → no reload" $
        let (_, acts) = transition testCfg (EvTransform "hello")
         in not (hasReload acts)
    , test "transform: plain query, wasRg → reload" $
        let cfg1 = testCfg{cWasRg = True}
            (_, acts) = transition cfg1 (EvTransform "hello")
         in hasReload acts
    , test "transform: #pattern → rg prompt, disable search, reload" $
        let (_, acts) = transition testCfg (EvTransform "#pattern")
         in hasAction (ChangePrompt "rg> ") acts
                && hasAction DisableSearch acts
                && hasReload acts
    , test "transform: #pat#filt → filter prompt" $
        let (_, acts) = transition testCfg (EvTransform "#pat#filt")
         in hasAction (ChangePrompt "filter> ") acts
    , test "transform: foo#bar → fzf#rg prompt" $
        let (_, acts) = transition testCfg (EvTransform "foo#bar")
         in hasAction (ChangePrompt "fzf#rg> ") acts
    , test "transform: foo# → fzf# prompt" $
        let (_, acts) = transition testCfg (EvTransform "foo#")
         in hasAction (ChangePrompt "fzf#> ") acts
    , test "transform: sets cWasRg for rg mode" $
        let (cfg', _) = transition testCfg (EvTransform "#pattern")
         in cWasRg cfg'
    , test "transform: clears cWasRg for file mode" $
        let cfg1 = testCfg{cWasRg = True}
            (cfg', _) = transition cfg1 (EvTransform "hello")
         in not (cWasRg cfg')
    , test "transform: auto-switches dirs→files on rg entry" $
        let cfg1 = testCfg{cFd = FdDirs}
            (cfg', acts) = transition cfg1 (EvTransform "#pattern")
         in cFd cfg' == FdFiles && hasFooter acts
    , test "transform: stays files on rg entry" $
        let (cfg', acts) = transition testCfg (EvTransform "#pattern")
         in cFd cfg' == FdFiles && not (hasFooter acts)
    , test "transform: dirs prompt in dirs mode" $
        let cfg1 = testCfg{cFd = FdDirs}
            (_, acts) = transition cfg1 (EvTransform "hello")
         in hasAction (ChangePrompt "dirs> ") acts
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- Transition: Swap
-- ═══════════════════════════════════════════════════════════════════════

transitionSwapTests :: [TestResult]
transitionSwapTests =
    [ test "swap: FileMode → append #" $
        let (_, acts) = transition testCfg (EvSwap "hello")
         in hasAction (ChangeQuery "hello#") acts
    , test "swap: RgLive → pat#" $
        let (_, acts) = transition testCfg (EvSwap "#pattern")
         in hasAction (ChangeQuery "pattern#") acts
    , test "swap: RgLocked → filt#pat" $
        let (_, acts) = transition testCfg (EvSwap "#pat#filt")
         in hasAction (ChangeQuery "filt#pat") acts
    , test "swap: FzfRg → #pat#filt" $
        let (_, acts) = transition testCfg (EvSwap "filt#pat")
         in hasAction (ChangeQuery "#pat#filt") acts
    , test "swap: FzfRgPending → #filt" $
        let (_, acts) = transition testCfg (EvSwap "filt#")
         in hasAction (ChangeQuery "#filt") acts
    , test "swap: does not modify config" $
        let (cfg', _) = transition testCfg (EvSwap "hello")
         in cfg' == testCfg
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- Transition: ExtraArgs
-- ═══════════════════════════════════════════════════════════════════════

transitionExtraArgsTests :: [TestResult]
transitionExtraArgsTests =
    [ test "extraArgs: #pattern → #pattern -- -" $
        let (_, acts) = transition testCfg (EvExtraArgs "#pattern")
         in acts == [ChangeQuery "#pattern -- -"]
    , test "extraArgs: #pat#filt → #pat -- -#filt" $
        let (_, acts) = transition testCfg (EvExtraArgs "#pat#filt")
         in acts == [ChangeQuery "#pat -- -#filt"]
    , test "extraArgs: #pat#filt with spaces → preserves filter" $
        let (_, acts) = transition testCfg (EvExtraArgs "#search term#my filter")
         in acts == [ChangeQuery "#search term -- -#my filter"]
    , test "extraArgs: foo#bar (fzfRg) → foo#bar -- -" $
        let (_, acts) = transition testCfg (EvExtraArgs "foo#bar")
         in acts == [ChangeQuery "foo#bar -- -"]
    , test "extraArgs: foo# (pending) → no-op" $
        let (_, acts) = transition testCfg (EvExtraArgs "foo#")
         in null acts
    , test "extraArgs: plain query → no-op" $
        let (_, acts) = transition testCfg (EvExtraArgs "hello")
         in null acts
    , test "extraArgs: empty → no-op" $
        let (_, acts) = transition testCfg (EvExtraArgs "")
         in null acts
    , test "extraArgs: does not modify config" $
        let (cfg', _) = transition testCfg (EvExtraArgs "#pat")
         in cfg' == testCfg
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- Transition: SmartEnter
-- ═══════════════════════════════════════════════════════════════════════

transitionSmartEnterTests :: [TestResult]
transitionSmartEnterTests =
    [ test "smartEnter: files mode, not alt → accept" $
        let (_, acts) = transition testCfg (EvSmartEnter False)
         in acts == [Accept]
    , test "smartEnter: files mode, alt → execute edit" $
        let (_, acts) = transition testCfg (EvSmartEnter True)
         in case acts of [Execute cmd] -> "edit" `T.isInfixOf` cmd; _ -> False
    , test "smartEnter: dirs mode → become navigate" $
        let cfg1 = testCfg{cFd = FdDirs}
            (_, acts) = transition cfg1 (EvSmartEnter False)
         in case acts of [Become cmd] -> "navigate" `T.isInfixOf` cmd; _ -> False
    , test "smartEnter: dirs mode, alt also navigates" $
        let cfg1 = testCfg{cFd = FdDirs}
            (_, acts) = transition cfg1 (EvSmartEnter True)
         in case acts of [Become cmd] -> "navigate" `T.isInfixOf` cmd; _ -> False
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- Transition: Query stack
-- ═══════════════════════════════════════════════════════════════════════

transitionQueryTests :: [TestResult]
transitionQueryTests =
    [ test "queryPush: adds to empty stack" $
        let (cfg', _) = transition testCfg (EvQueryPush "hello")
         in cQueryStack cfg' == ["hello"]
    , test "queryPush: prepends to stack" $
        let cfg1 = testCfg{cQueryStack = ["old"]}
            (cfg', _) = transition cfg1 (EvQueryPush "new")
         in cQueryStack cfg' == ["new", "old"]
    , test "queryPush: dedup against top" $
        let cfg1 = testCfg{cQueryStack = ["same", "other"]}
            (cfg', _) = transition cfg1 (EvQueryPush "same")
         in cQueryStack cfg' == ["same", "other"]
    , test "queryPush: empty query is no-op" $
        let (cfg', _) = transition testCfg (EvQueryPush "")
         in null (cQueryStack cfg')
    , test "queryPush: no actions emitted" $
        let (_, acts) = transition testCfg (EvQueryPush "q")
         in null acts
    , test "queryDelete: removes from stack" $
        let cfg1 = testCfg{cQueryStack = ["a", "b", "c"]}
            (cfg', _) = transition cfg1 (EvQueryDelete "b")
         in cQueryStack cfg' == ["a", "c"]
    , test "queryDelete: non-existent is no-op" $
        let cfg1 = testCfg{cQueryStack = ["a", "b"]}
            (cfg', _) = transition cfg1 (EvQueryDelete "x")
         in cQueryStack cfg' == ["a", "b"]
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- renderActions
-- ═══════════════════════════════════════════════════════════════════════

renderActionsTests :: [TestResult]
renderActionsTests =
    [ test "renderActions: single action" $
        renderActions [ChangePrompt "rg> "] == "change-prompt(rg> )"
    , test "renderActions: multiple actions joined with +" $
        renderActions [ChangePrompt "rg> ", DisableSearch]
            == "change-prompt(rg> )+disable-search"
    , test "renderActions: empty list" $
        renderActions [] == ""
    , test "renderActions: accept" $
        renderActions [Accept] == "accept"
    , test "renderActions: become" $
        renderActions [Become "fzfx --navigate into {} {q}"]
            == "become:fzfx --navigate into {} {q}"
    , test "renderActions: execute" $
        renderActions [Execute "fzfx --edit {}"]
            == "execute(fzfx --edit {})"
    , test "renderActions: reload-sync" $
        renderActions [ReloadSync "fzfx --reload {q}"]
            == "reload-sync(fzfx --reload {q})"
    , test "renderActions: change-border-label" $
        renderActions [ChangeFooter "status text"]
            == "change-border-label(status text)"
    , test "renderActions: change-preview-window" $
        renderActions [ChangePreviewWindow "bottom:50%"]
            == "change-preview-window(bottom:50%)"
    , -- fzfWrap delimiter selection
      test "fzfWrap: normal text uses parens" $
        fzfWrap "change-query" "hello" == "change-query(hello)"
    , test "fzfWrap: text with ) uses brackets" $
        fzfWrap "change-query" "foo)" == "change-query[foo)]"
    , test "fzfWrap: text with ) and ] uses angle brackets" $
        fzfWrap "change-query" "foo)]" == "change-query<foo)]>"
    , test "fzfWrap: text with ) ] > uses tildes" $
        fzfWrap "change-query" "foo)]>" == "change-query~foo)]>~"
    , test "renderActions: query with paren is safe" $
        let acts = renderActions [ChangeQuery "test)query"]
         in "change-query[test)query]" == acts
    , test "renderActions: query with + inside delimiters is safe" $
        let acts = renderActions [ChangeQuery "a+b"]
         in "change-query(a+b)" == acts
    ]
