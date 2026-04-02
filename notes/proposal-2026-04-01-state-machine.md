# Proposal: State machine architecture for fzfx transitions

## Status

Draft

## Problem

State transitions in fzfx are scattered across IO handlers (`cmdToggle`, `cmdTransform`, `cmdSwap`, `cmdSmartEnter`, `cmdNavigate`) that each:

1. Load config from disk (`loadConfig` / `modConfig`)
2. Compute new state inline
3. Write config back to disk
4. Emit fzf action strings to stdout

This makes transitions hard to test — you can't call `cmdToggle "hidden"` in a unit test without a real config file, environment variables, and stdout capture. The logic for "what state changes" is tangled with "how to persist state" and "what fzf actions to emit."

Concrete symptoms:
- The `cmdToggle` handler has 7 branches, each computing a different config delta + fzf action string. None are unit-tested.
- `cmdTransform` calls `modConfig` twice (once for `cFd`, once for `cWasRg`) then computes the fzf action from the *original* config, creating subtle sequencing bugs.
- The navigate bug in `bugs-2026-04-01.md` (hung process) is hard to reproduce because the transition logic lives inside IO.
- `cmdToggle "type_toggle"` recurses into `cmdToggle "type_d"` / `cmdToggle "type_f"` — a code smell for what should be a simple state flip.

## Context

The current `Fzfx.Core` module (just extracted) holds all pure types and parsing functions. The natural next step is to make transitions pure too.

fzf's `transform:` binding protocol is essentially an event loop: fzf sends an event (keystroke + context), fzfx computes the response (fzf action string), and fzf applies it. This maps cleanly to a state machine where each handler is `(Config, Event) → (Config, [FzfAction])`.

**Prior art:**
- Elm / TEA (The Elm Architecture): `update : Msg -> Model -> (Model, Cmd Msg)`
- Brick (Haskell TUI): `appHandleEvent : BrickEvent n e -> EventM n s ()`
- Redux: `reducer(state, action) → state`

All separate "what changes" from "how to execute side effects."

---

## Approach A: Pure transition function in Core

### Architecture

Add an `Event` type and a pure `transition` function to `Fzfx.Core`:

```haskell
-- Events that trigger state changes
data Event
    = EvToggle ToggleName Text  -- toggle name, current query
    | EvTransform Text          -- query changed
    | EvSwap Text               -- swap query format
    | EvSmartEnter Bool         -- is alt-enter?
    | EvQueryPush Text          -- save query to stack
    | EvQueryDelete Text        -- remove query from stack
    | EvSelSave Text [Text]     -- mode, paths
    deriving (Eq, Show)

data ToggleName = TgAtPrefix | TgDiff | TgHidden | TgNoIgnore | TgType
    deriving (Eq, Show)

-- FZF actions to emit (composable)
data FzfAction
    = ChangePrompt Text
    | ChangeQuery Text
    | ChangeHeader Text
    | ReloadSync Text           -- command to run
    | EnableSearch
    | DisableSearch
    | RefreshPreview
    | First                     -- jump to first item
    | Accept
    | Become Text               -- become: command
    | Execute Text              -- execute(): command
    deriving (Eq, Show)

-- Pure transition: old config → event → (new config, actions to emit)
transition :: Config -> Event -> (Config, [FzfAction])
transition cfg (EvToggle TgHidden _) =
    let cfg' = cfg { cHid = not (cHid cfg) }
    in (cfg', [reloadAction cfg', headerAction cfg'])
transition cfg (EvToggle TgDiff _) =
    let cfg' = cfg { cPrev = if cPrev cfg == Content then Diff else Content }
    in (cfg', [RefreshPreview, headerAction cfg'])
-- ... etc

-- Render actions to fzf protocol string
renderActions :: [FzfAction] -> Text
renderActions = T.intercalate "+" . map render1
  where
    render1 (ChangePrompt p) = "change-prompt(" <> p <> ")"
    render1 (ChangeQuery q)  = "change-query(" <> q <> ")"
    render1 (ReloadSync cmd) = "reload-sync(" <> cmd <> ")"
    -- ... etc
```

The IO handlers become thin wrappers:

```haskell
cmdToggle :: Text -> IO ()
cmdToggle nameAndArgs = do
    cfg <- loadConfig
    let (name, curQ) = T.breakOn " " nameAndArgs
        evt = EvToggle (parseToggleName name) (T.drop 1 curQ)
        (cfg', actions) = transition cfg evt
    saveConfig cfg'
    TIO.putStr (renderActions actions)
    hFlush stdout
```

### Trade-offs

**Strengths:**
- Every transition is testable as a pure function: `transition cfg event == (expectedCfg, expectedActions)`
- Bugs like the double-`modConfig` in `cmdTransform` become impossible — one atomic state update
- The `FzfAction` type serves as documentation of what fzf actions exist
- `renderActions` can be tested independently
- Navigate bugs can be reproduced in unit tests with specific Config values
- The `type_toggle` recursion disappears — it's just a pattern match

**Weaknesses:**
- Some handlers need IO *between* steps (e.g., `cmdNavigate` calls `doesDirectoryExist`, `fzf --filter` for chunk matching). These can't be pure transitions without either:
  - Splitting into pre-IO (gather data) → pure transition → post-IO (emit)
  - Or using an effect type / free monad (heavy)
- `hdrText` renders ANSI-colored headers — moves to Core or stays in Main with the action referencing it

---

## Approach B: Effect-tagged transitions (pre/pure/post split)

### Architecture

Same `Event`/`FzfAction` types, but handlers that need IO use a three-phase pattern:

```haskell
-- Phase 1: Pure pre-computation (what IO do we need?)
data IORequest
    = CheckDir FilePath
    | FzfFilter Text [Text]     -- filter pattern, input lines
    | DetectGit Text
    deriving (Eq, Show)

-- Phase 2: Pure transition with resolved IO results
data ResolvedEvent
    = EvNavigate NavAction Text Text Bool  -- action, newCwd, query, targetIsDir
    | EvSimple Event                       -- no IO needed
    deriving (Eq, Show)

-- The pure core
transition :: Config -> ResolvedEvent -> (Config, [FzfAction])

-- IO handlers do the plumbing
cmdNavigate :: Text -> Text -> Text -> IO ()
cmdNavigate navAction sel query = do
    cfg <- loadConfig
    -- Phase 1: gather IO
    let target = computeTarget cfg navAction sel
    isDir <- doesDirectoryExist (t target)
    newGit <- detectGit target
    -- Phase 2: pure transition
    let resolved = EvNavigate navAction target query isDir
        (cfg', actions) = transition cfg resolved
    -- Phase 3: persist + emit
    saveConfig cfg'
    TIO.putStr (renderActions actions)
```

### Trade-offs

**Strengths:**
- Handles the navigate/IO case cleanly without free monads
- Every transition is still pure and testable — just pass in the resolved values
- Test: "if user navigates into 'sub' and it is a directory, config should update cwd and actions should include reload"
- No new abstractions beyond what Approach A already introduces

**Weaknesses:**
- Slightly more boilerplate (two event types)
- The pre-computation phase is handler-specific and not itself unit-tested (but it's just IO plumbing)

---

## Approach C: Full effect interpreter (free monad / operational)

### Architecture

Encode all effects as a DSL, interpret purely for tests:

```haskell
data FzfxF next
    = LoadConfig (Config -> next)
    | SaveConfig Config next
    | CheckDir FilePath (Bool -> next)
    | Emit [FzfAction] next
    | ...
    deriving Functor

type Fzfx = Free FzfxF
```

### Trade-offs

**Strengths:**
- Total purity — entire handler logic is testable
- Can swap interpreters (test vs production)

**Weaknesses:**
- Massive overkill for this project — adds `free` or `operational` dependency
- Obscures simple logic behind monadic plumbing
- Every new IO operation needs a new constructor
- Hard to justify for a ~1300 line tool

---

## Approach comparison

| Criterion              | A: Pure transition   | B: Pre/pure/post     | C: Free monad        |
|------------------------|----------------------|----------------------|----------------------|
| Testability            | High (simple cases)  | High (all cases)     | Total                |
| Handles IO in handlers | No (split required)  | Yes (clean pattern)  | Yes (encoded)        |
| Complexity added       | Low                  | Low-medium           | High                 |
| New dependencies       | None                 | None                 | `free` or similar    |
| Fits project scale     | Yes                  | Yes                  | No                   |
| Incremental adoption   | Easy (one handler)   | Easy (one handler)   | All-or-nothing       |

---

## Open questions

1. Should `hdrText` move to Core (it's pure but couples to ANSI rendering) or should `ChangeHeader` actions carry the rendered text?
2. Should `renderActions` live in Core (testable) or Main (closer to fzf protocol)?
3. `cmdNavigate` currently calls `mainLaunch` (restarts fzfx via become). Should this be modeled as a special `FzfAction` or stay as IO?
4. The `statusToggle` bindings emit raw shell script — should those be modeled as actions or left as-is (they're fzf-side, not state transitions)?

## Recommendation

**Approach B (pre/pure/post split)** is the better fit.

Justification:
1. Approach A alone can't handle `cmdNavigate` (needs `doesDirectoryExist`, `fzf --filter`, `detectGit`), and navigate is the buggiest handler per the known issues. Leaving it out of the testable core defeats the purpose.
2. Approach B is a superset of A — simple handlers use `EvSimple` wrapping, IO handlers use `ResolvedEvent`. No wasted abstraction.
3. Approach C is architecturally clean but adds dependency weight and cognitive overhead that doesn't match a 1300-line tool. The pre/pure/post split achieves the same testability for the cases that matter.
4. Both A and B can be adopted incrementally (one handler at a time), but B provides a consistent pattern for all handlers from the start.

### Implementation phases

**Phase 1: Core types + simple transitions**
- Add `Event`, `ToggleName`, `FzfAction`, `renderActions` to `Fzfx.Core`
- Implement `transition` for the simple toggle cases (at_prefix, diff, hidden, no_ignore, type)
- Port `cmdToggle` to use `transition` — one thin IO wrapper
- Add unit tests: for each toggle, assert config delta and action list
- Verify with `nix build` + integration tests

**Phase 2: Transform + Swap**
- Add `EvTransform`, `EvSwap` to `Event`
- Implement transform transition (mode switching logic, prompt changes)
- Port `cmdTransform` and `cmdSwap`
- Tests: mode switch from FileMode→RgLive, RgLive→FileMode, auto-switch from dirs to files on rg entry

**Phase 3: Navigate (pre/pure/post)**
- Add `ResolvedEvent` with `EvNavigate`
- Implement navigate transition (cwd update, query chunk filtering, mode flip)
- Port `cmdNavigate` with pre-IO gather phase
- Tests: navigate into dir, navigate up, navigate to root, query chunk removal

**Phase 4: Selection + Query stack**
- Port `cmdSmartEnter`, `cmdSelSave`, `cmdSelRestore`, `cmdQueryPush`, `cmdQueryDelete`
- These are simpler — mostly config mutations with straightforward actions
