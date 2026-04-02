{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
main = runTests $ concat
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
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- tryRg
-- ═══════════════════════════════════════════════════════════════════════

tryRgTests :: [TestResult]
tryRgTests =
    [ test "tryRg basic" $
        tryRg "src/Main.hs:42:10:some code" == Just ("src/Main.hs", 42)
    , test "tryRg with colons in text" $
        tryRg "file.hs:17:5:key: value" == Just ("file.hs", 17)
    , test "tryRg with multiple colons in text" $
        tryRg "file.hs:1:1:a:b:c" == Just ("file.hs", 1)
    , test "tryRg no match on plain filename" $
        tryRg "CLAUDE.md" == Nothing
    , test "tryRg no match on fd line" $
        tryRg " \tCLAUDE.md" == Nothing
    , test "tryRg empty" $
        tryRg "" == Nothing
    , test "tryRg only colons" $
        tryRg ":::" == Nothing
    , test "tryRg non-digit line number" $
        tryRg "file:abc:1:text" == Nothing
    , test "tryRg non-digit col" $
        tryRg "file:1:abc:text" == Nothing
    , test "tryRg trailing colon (empty text)" $
        tryRg "file:1:1:" == Just ("file", 1)
    , test "tryRg path with dots" $
        tryRg "./src/Foo.hs:10:3:code" == Just ("./src/Foo.hs", 10)
    , test "tryRg large line number" $
        tryRg "f:99999:1:x" == Just ("f", 99999)
    , test "tryRg single char file" $
        tryRg "f:1:1:x" == Just ("f", 1)
    , test "tryRg missing text after col" $
        tryRg "f:1:1" == Nothing
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
        parseLine "src/Main.hs:42:10:code here" == RgLine "src/Main.hs" 42
    , test "parseLine rg with ANSI" $
        parseLine "\ESC[35msrc/Main.hs\ESC[0m:42:10:code" == RgLine "src/Main.hs" 42
    , test "parseLine rg with colons in text" $
        parseLine "file.hs:17:5:key: value" == RgLine "file.hs" 17
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
        lineFile (RgLine "src/Main.hs" 42) == "src/Main.hs"
    , test "lineFile FdLine" $
        lineFile (FdLine Clean "README.md") == "README.md"
    , test "lineRef RgLine" $
        lineRef (RgLine "src/Main.hs" 42) == "src/Main.hs:42"
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
         in T.isPrefixOf "/" result  -- should be absolute
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
        interleave [] ([] :: [Text]) == []
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
        ordNub [] == []
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
    <>
    [ test "parseSubcmd unknown flag" $
        parseSubcmd "--nonexistent" == Nothing
    , test "parseSubcmd empty" $
        parseSubcmd "" == Nothing
    ]

-- ═══════════════════════════════════════════════════════════════════════
-- Config Read/Show roundtrip
-- ═══════════════════════════════════════════════════════════════════════

configRoundtripTests :: [TestResult]
configRoundtripTests =
    [ test "Config Read/Show roundtrip" $
        let cfg = Config
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
                , cPrev = Diff
                , cFileQuery = "some query"
                , cDirQuery = ""
                , cQueryStack = ["#foo", "bar#baz"]
                , cPendingQuery = ""
                , cWasRg = True
                , cSavedFileSel = ["/home/user/proj/a.hs"]
                , cSavedDirSel = []
                }
         in read (show cfg) == cfg
    , test "Config Read/Show roundtrip default-like" $
        let cfg = Config
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
                , cPrev = Content
                , cFileQuery = ""
                , cDirQuery = ""
                , cQueryStack = []
                , cPendingQuery = ""
                , cWasRg = False
                , cSavedFileSel = []
                , cSavedDirSel = []
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
