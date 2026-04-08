# fzfx

An FZF-based file picker with ripgrep integration, directory navigation, git
awareness, and tmux output support. Written in Haskell.

fzfx unifies file finding (`fd`), content search (`rg`), and directory
browsing into a single interactive interface powered by `fzf`. It supports
modal switching between files, directories, and mixed views, with live preview,
git status filtering, query history, and saved selections.

## Features

- **File search** via `fd` with hidden/ignored file toggles
- **Live ripgrep** search with `#pattern` query syntax
- **Directory navigation** — drill in/out, jump to git root, zoxide integration
- **Git awareness** — filter by status (unstaged/staged/untracked), diff preview via `delta`
- **Preview** — syntax-highlighted file preview (`bat`), directory trees (`eza`), git diffs
- **Multi-select** with selection save/restore across mode switches
- **Query history stack** — push, pop, and browse previous queries
- **tmux integration** — send selected paths to a target pane
- **Bash completion** — smart context-aware path completion

## Requirements

**Runtime dependencies:**

| Tool                                          | Purpose              |
| --------------------------------------------- | -------------------- |
| [fzf](https://github.com/junegunn/fzf)       | Interactive filter    |
| [fd](https://github.com/sharkdp/fd)          | File finding          |
| [ripgrep](https://github.com/BurntSushi/ripgrep) | Content search   |
| [bat](https://github.com/sharkdp/bat)        | Syntax preview        |
| [eza](https://github.com/eza-community/eza)  | Directory tree        |
| [delta](https://github.com/dandavison/delta)  | Git diff preview     |
| [git](https://git-scm.com)                    | Git integration      |

**Optional:**

| Tool                                              | Purpose              |
| ------------------------------------------------- | -------------------- |
| [tmux](https://github.com/tmux/tmux)              | Pane output          |
| [zoxide](https://github.com/ajeetdsouza/zoxide)   | Directory jumping    |
| [tokei](https://github.com/XAMPPRocky/tokei)      | Code statistics      |

**Build dependencies:** GHC 9.6+ and Cabal, or Nix.

## Installation

### With Nix (recommended)

```bash
nix build github:tavisrudd/fzfx
# or add to your flake inputs
```

### With Cabal

```bash
git clone https://github.com/tavisrudd/fzfx.git
cd fzfx
cabal build
cabal install
```

## Usage

```bash
fzfx                    # pick files, output to stdout
fzfx -t d               # directory mode
fzfx -t m               # mixed files + directories
fzfx --cwd ~/projects   # start in a specific directory
fzfx -o tmux            # send selection to tmux pane
fzfx --git-status       # show only git-modified files
fzfx "query"            # start with an initial query
```

### Query syntax

| Pattern            | Behavior                         |
| ------------------ | -------------------------------- |
| `text`             | File search via `fd`             |
| `#pattern`         | Live ripgrep content search      |
| `#pat#filter`      | Ripgrep then filter results      |
| `\#text`           | Literal `#` in file search       |

### Key bindings

| Key             | Action                              |
| --------------- | ----------------------------------- |
| `enter`         | Select and output                   |
| `alt-enter`     | Open in `$EDITOR`                   |
| `tab`           | Toggle selection                    |
| `alt-3`         | Switch to ripgrep mode              |
| `ctrl-t`        | Cycle files/dirs/mixed              |
| `ctrl-o`        | Navigate into directory             |
| `ctrl-l`        | Navigate up                         |
| `ctrl-r`        | Jump to git root                    |
| `alt-z`         | Zoxide jump                         |
| `alt-g`         | Toggle git status filter            |
| `ctrl-alt-g`    | Toggle diff preview                 |
| `alt-h`         | Toggle hidden files                 |
| `alt-p`         | Toggle preview                      |
| `ctrl-p`        | Toggle preview layout (right/bottom)|
| `alt-/`         | Full-screen preview in pager        |
| `f4`            | Push query to stack                 |
| `f3`            | Browse/restore query stack          |
| `ctrl-h`        | Show keybinding help                |

See `fzfx --help` for the full list.

### Bash completion

Source the completion script after [fzf-bash-completion](https://github.com/lincheney/fzf-tab-completion):

```bash
source /path/to/fzfx/shell/bash-completion.bash
```

This binds `alt-/` to smart path completion using fzfx.

## Architecture

The core state machine (`src/Fzfx/Core.hs`) is pure — all transitions are
deterministic functions over an immutable state type, with effects handled
separately in `src/Main.hs`. This makes the logic easy to test and reason
about.

## License

[MIT](LICENSE)
