{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (exitFailure, exitSuccess)

-- Inline the pure functions under test (since Main doesn't export them)

data SearchMode
    = FileMode
    | RgLive Text [Text]
    | RgLocked Text Text [Text]
    | FzfRg Text Text [Text]
    | FzfRgPending Text
    deriving (Show)

data GitStatus = Unstaged | Staged | Untracked | Clean
    deriving (Eq, Show)

data LineInfo
    = RgLine Text Int
    | FdLine GitStatus Text
    deriving (Eq, Show)

-- tryRg: parse from the left
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
    isAllDigit s' = not (T.null s') && T.all (\c -> c >= '0' && c <= '9') s'

stripAnsi :: Text -> Text
stripAnsi txt
    | T.null txt = txt
    | T.head txt == '\ESC' = case T.uncons (T.tail txt) of
        Just ('[', rest) -> stripAnsi (T.drop 1 (T.dropWhile (\c -> c < '@' || c > '~') rest))
        Just (_, rest) -> stripAnsi rest
        Nothing -> txt
    | otherwise = T.cons (T.head txt) (stripAnsi (T.tail txt))

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

-- Test runner
data TestResult = Pass | Fail String

test :: String -> Bool -> TestResult
test name True = Pass
test name False = Fail name

runTests :: [TestResult] -> IO ()
runTests results = do
    let failures = [msg | Fail msg <- results]
        total = length results
        failed = length failures
    mapM_ (\msg -> putStrLn $ "  FAIL: " <> msg) failures
    putStrLn $ show (total - failed) <> "/" <> show total <> " passed"
    if null failures then exitSuccess else exitFailure

main :: IO ()
main = runTests
    -- tryRg: basic rg line
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

    -- stripAnsi
    , test "stripAnsi plain" $
        stripAnsi "hello" == "hello"
    , test "stripAnsi with color" $
        stripAnsi "\ESC[31mred\ESC[0m" == "red"
    , test "stripAnsi with bold" $
        stripAnsi "\ESC[1;33mfoo\ESC[0m:bar" == "foo:bar"

    -- parseLine
    , test "parseLine rg line" $
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

    -- parseQuery
    , test "parseQuery plain text" $
        case parseQuery "hello" of FileMode -> True; _ -> False
    , test "parseQuery escaped hash" $
        case parseQuery "\\#hello" of FileMode -> True; _ -> False
    , test "parseQuery rg live" $
        case parseQuery "#pattern" of RgLive "pattern" [] -> True; _ -> False
    , test "parseQuery rg locked" $
        case parseQuery "#pattern#filter" of RgLocked "pattern" "filter" [] -> True; _ -> False
    , test "parseQuery rg locked empty filter" $
        case parseQuery "#pattern#" of RgLocked "pattern" "" [] -> True; _ -> False
    , test "parseQuery rg with extra args" $
        case parseQuery "#pat -- -g *.hs" of RgLive "pat" ["-g", "*.hs"] -> True; _ -> False
    , test "parseQuery fzfRg" $
        case parseQuery "nix#import" of FzfRg "nix" "import" [] -> True; _ -> False
    , test "parseQuery fzfRg with extra args" $
        case parseQuery "nix#pat -- -g *.hs" of FzfRg "nix" "pat" ["-g", "*.hs"] -> True; _ -> False
    , test "parseQuery fzfRg trailing # pending" $
        case parseQuery "nix#" of FzfRgPending "nix" -> True; _ -> False
    , test "parseQuery fzfRg trailing # space pending" $
        case parseQuery "nix# " of FzfRgPending "nix" -> True; _ -> False
    ]
