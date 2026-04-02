# Proposal: fzfx test suite design

## Status

Draft

## Problem

The test suite (`test/Test.hs`, 179 lines) covers 4 parsing functions with ~28 hand-written assertions. Two issues:

1. **Duplicated code**: Pure functions (`tryRg`, `stripAnsi`, `parseLine`, `parseQuery`) are copy-pasted from `src/Main.hs` into the test file. Changes to Main won't be caught — tests pass against stale copies.
2. **Low coverage**: Many testable pure functions have zero tests: `parseSFilter`, `makeRelPath`, `interleave`, `ordNub`, `lineFile`/`lineRef`, `gitStatusChar`, `parseSubcmd`/`flg` roundtrip, and the `Config` Read/Show roundtrip.

## Context

- fzfx is a single-file Haskell executable (~1280 lines) with `module Main (main) where` — nothing is exported.
- The cabal file has a separate `test-suite` stanza that depends only on `base` and `text`.
- The test runner is a 15-line custom framework (no external deps). It works fine for what it does.
- Build uses `callCabal2nix` in the flake — library changes are automatically picked up.

---

## Approach A: Internal library via cabal

### Architecture

Split the cabal package into an **internal library** + executable + test-suite. Move all pure types and functions into a library module; Main re-exports and adds the IO machinery.

```
fzfx.cabal changes:
  library fzfx-internal
    exposed-modules: Fzfx.Core
    hs-source-dirs:  src
    ...

  executable fzfx
    main-is:       Main.hs
    hs-source-dirs: src
    build-depends: fzfx:fzfx-internal, ...

  test-suite fzfx-test
    main-is:       Test.hs
    hs-source-dirs: test
    build-depends: fzfx:fzfx-internal, base, text
```

New file `src/Fzfx/Core.hs` exports the pure functions. `src/Main.hs` imports from `Fzfx.Core` instead of defining them inline. Test file imports `Fzfx.Core` directly — no more duplication.

### Trade-offs

**Strengths:**
- Eliminates duplication permanently — tests always run against real code
- Standard Haskell pattern; `callCabal2nix` handles it without changes
- Opens the door for property tests (QuickCheck) since the test suite can depend on the library
- No new files beyond `src/Fzfx/Core.hs`

**Weaknesses:**
- Requires restructuring `Main.hs` (moving ~200 lines of pure code out, adding imports)
- Internal libraries need `cabal-version: 3.0` (currently 2.4) — minor bump
- Slightly more complex cabal file

---

## Approach B: Keep duplication, expand inline tests

### Architecture

Keep the current structure. Add the missing functions as more copy-paste blocks in `test/Test.hs`. Add a CI/review discipline to keep copies in sync.

### Trade-offs

**Strengths:**
- Zero structural changes — just append tests
- Works today, right now

**Weaknesses:**
- Duplication doubles from ~80 lines to ~150+ lines
- `makeRelPath` and other functions have local helpers (`joinPath`, `dropTrailingSep`) that must also be duplicated
- Silent drift remains the core risk — a bug fix in Main that isn't mirrored in Test means tests lie
- Can't add property tests (QuickCheck) without the library split anyway

---

## Approach comparison

| Criterion               | A: Internal library          | B: Inline duplication        |
|--------------------------|------------------------------|------------------------------|
| Eliminates drift risk    | Yes                          | No                           |
| Structural change needed | Moderate (split + cabal)     | None                         |
| Future extensibility     | QuickCheck, HSpec, etc.      | Limited by duplication       |
| Nix flake changes        | None (callCabal2nix handles) | None                         |
| Time to implement        | ~1 hour                      | ~30 min                      |

---

## Open questions

1. Should the library module be `Fzfx.Core` (single file) or split further (e.g. `Fzfx.Parse`, `Fzfx.Path`)? Single file keeps it simple for a ~200 line extraction.
2. Keep the custom test runner or switch to Hspec/HUnit? The custom runner is fine for now — switching frameworks is orthogonal and can be done later.
3. Should `Config` and its Read/Show serialization move to the library? It's technically pure but tightly coupled to the IO code. Recommend yes — the roundtrip property is valuable to test.

## Recommendation

**Approach A (internal library)** is the better fit.

Justification:
1. The duplication already caused the test file to use `Data.Char.isDigit` vs a hand-rolled digit check — the copies have already diverged subtly. This will only get worse.
2. `makeRelPath` is the function most likely to have edge-case bugs (path separator handling, `..` capping) and it's untested. Testing it requires duplicating its helpers too — fragile.
3. The cabal-version bump from 2.4→3.0 is trivial and `callCabal2nix` handles internal libraries correctly.
4. Once the library exists, adding QuickCheck for the parsing functions is a natural next step (but not required for phase 1).

### Implementation phases

**Phase 1: Extract and test**
- Create `src/Fzfx/Core.hs` with all pure types + functions
- Update `src/Main.hs` to import from `Fzfx.Core`
- Update `fzfx.cabal` (bump cabal-version, add internal library)
- Rewrite `test/Test.hs` to import `Fzfx.Core` (delete duplicated code)
- Add tests for: `parseSFilter`, `makeRelPath`, `interleave`, `ordNub`, `lineFile`/`lineRef`, `gitStatusChar`, `flg`/`parseSubcmd` roundtrip, `Config` Read/Show roundtrip
- Verify `cabal test` and `nix build` both pass

**Phase 2: Property tests (optional, later)**
- Add QuickCheck dependency
- Property tests for `stripAnsi` (idempotent), `parseLine`/`tryRg` (never crashes), `makeRelPath` (output is valid relative path or absolute), `interleave` (preserves all elements, maintains sort order)
