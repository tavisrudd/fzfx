#!/usr/bin/env bash
# fzfx integration tests using a dedicated tmux server socket.
# Launches fzfx in a controlled tmux session, sends keys, and asserts
# on captured pane content.
#
# Usage: ./test/integration.sh [path-to-fzfx-binary]
#   Defaults to ./result/bin/fzfx (nix build output)
#
# Requirements: tmux, fzf, fd, rg, bat, eza (runtime deps of fzfx)

set -uo pipefail

FZFX="${1:-./result/bin/fzfx}"
TSOCK="/tmp/fzfx-test-tmux-$$"
TSRV="tmux -S $TSOCK"
TEST_DIR=""
PASSED=0
FAILED=0
FAILURES=()

# ═══════════════════════════════════════════════════════════════════════
# Helpers
# ═══════════════════════════════════════════════════════════════════════

cleanup() {
    $TSRV kill-server 2>/dev/null || true
    rm -f "$TSOCK"
    if [[ -n "$TEST_DIR" ]]; then
        rm -rf "$TEST_DIR"
    fi
}
trap cleanup EXIT

setup_test_dir() {
    TEST_DIR="$(mktemp -d /tmp/fzfx-integ-XXXXXX)"
    mkdir -p "$TEST_DIR/src" "$TEST_DIR/lib" "$TEST_DIR/sub/deep/bottom"
    echo 'main = putStrLn "hello"' > "$TEST_DIR/src/Main.hs"
    echo 'module Lib where'        > "$TEST_DIR/lib/Lib.hs"
    echo 'import Data.Text'        > "$TEST_DIR/src/Other.hs"
    echo 'readme content'          > "$TEST_DIR/README.md"
    echo 'nested file'             > "$TEST_DIR/sub/deep/nested.txt"
    echo 'bottom file'             > "$TEST_DIR/sub/deep/bottom/floor.txt"
    echo 'sub-level file'          > "$TEST_DIR/sub/mid.txt"
    echo '.hidden content'         > "$TEST_DIR/.hidden"
    # Init a git repo so fzfx can detect git status
    (cd "$TEST_DIR" && git init -q && git -c commit.gpgsign=false commit --allow-empty -q -m "init" && git add -A && git -c commit.gpgsign=false commit -q -m "add files")
    # Create an unstaged change
    echo 'modified' >> "$TEST_DIR/README.md"
}

# Launch fzfx in a tmux session. Args are passed to fzfx.
launch_fzfx() {
    $TSRV kill-server 2>/dev/null || true
    $TSRV new-session -d -s test -x 220 -y 40 \
        -e "_FZFX_OUTPUT_MODE=stdout" \
        -e "_FZFX_CWD=$TEST_DIR" \
        "$FZFX" "$@"
    # Give fzfx time to start and render
    sleep 1.2
}

# Capture the current pane content, stripping trailing whitespace
CAPTURE_FILE=""

capture() {
    CAPTURE_FILE="$(mktemp /tmp/fzfx-cap-XXXXXX)"
    $TSRV capture-pane -t test -p > "$CAPTURE_FILE" 2>/dev/null
}

# Send keys to the test session
send() {
    $TSRV send-keys -t test "$@"
}

# Wait for pane content to contain a pattern (up to N seconds)
wait_for() {
    local pattern="$1"
    local timeout="${2:-3}"
    local i=0
    while true; do
        capture
        if grep -qF "$pattern" "$CAPTURE_FILE"; then
            rm -f "$CAPTURE_FILE"
            return 0
        fi
        rm -f "$CAPTURE_FILE"
        sleep 0.2
        i=$((i + 1))
        if [[ $i -ge $((timeout * 5)) ]]; then
            return 1
        fi
    done
}

# Assert that captured pane contains a pattern
assert_contains() {
    local label="$1"
    local pattern="$2"
    capture
    if grep -qF "$pattern" "$CAPTURE_FILE"; then
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
        FAILURES+=("$label: expected pane to contain '$pattern'")
        echo "  FAIL: $label" >&2
        echo "  --- pane content ---" >&2
        head -20 "$CAPTURE_FILE" >&2
        echo "  ---" >&2
    fi
    rm -f "$CAPTURE_FILE"
}

# Assert that captured pane does NOT contain a pattern
assert_not_contains() {
    local label="$1"
    local pattern="$2"
    capture
    if ! grep -qF "$pattern" "$CAPTURE_FILE"; then
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
        FAILURES+=("$label: expected pane NOT to contain '$pattern'")
        echo "  FAIL: $label" >&2
    fi
    rm -f "$CAPTURE_FILE"
}

# Assert the prompt line contains a pattern
assert_prompt() {
    local label="$1"
    local pattern="$2"
    capture
    if grep -qF "$pattern" "$CAPTURE_FILE"; then
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
        FAILURES+=("$label: expected prompt '$pattern'")
        echo "  FAIL: $label" >&2
        echo "  --- pane content ---" >&2
        head -5 "$CAPTURE_FILE" >&2
        echo "  ---" >&2
    fi
    rm -f "$CAPTURE_FILE"
}

# ═══════════════════════════════════════════════════════════════════════
# Tests
# ═══════════════════════════════════════════════════════════════════════

test_basic_launch() {
    echo "# test_basic_launch"
    launch_fzfx
    wait_for "mixed>" 3 || true
    assert_prompt "shows mixed prompt" "mixed>"
    assert_contains "lists src/Main.hs" "Main.hs"
    assert_contains "lists README.md" "README.md"
    send "C-g"  # abort
    sleep 0.3
}

test_file_filtering() {
    echo "# test_file_filtering"
    launch_fzfx
    wait_for "mixed>" 3 || true
    send "Main"
    sleep 0.5
    assert_contains "filters to Main.hs" "Main.hs"
    # Other.hs should also match if fzf finds it, but README shouldn't be highlighted
    send "C-g"
    sleep 0.3
}

test_rg_mode_switch() {
    echo "# test_rg_mode_switch"
    launch_fzfx
    wait_for "mixed>" 3 || true
    # Clear query and type rg query
    send "C-u"
    sleep 0.2
    send "#putStrLn"
    sleep 1
    assert_prompt "switches to rg prompt" "rg>"
    assert_contains "finds putStrLn in Main.hs" "Main.hs"
    send "C-g"
    sleep 0.3
}

test_rg_locked_mode() {
    echo "# test_rg_locked_mode"
    launch_fzfx
    wait_for "mixed>" 3 || true
    send "C-u"
    sleep 0.2
    send "#import#"
    sleep 1
    assert_prompt "switches to filter prompt" "filter>"
    send "C-g"
    sleep 0.3
}

test_toggle_hidden() {
    echo "# test_toggle_hidden"
    launch_fzfx
    wait_for "mixed>" 3 || true
    assert_not_contains "hidden file not shown initially" ".hidden"
    send "M-h"  # toggle hidden
    sleep 0.8
    assert_contains "hidden file shown after toggle" ".hidden"
    send "C-g"
    sleep 0.3
}

test_toggle_dirs_mode() {
    echo "# test_toggle_dirs_mode"
    launch_fzfx
    wait_for "mixed>" 3 || true
    send "C-t"  # toggle to dirs
    sleep 0.8
    assert_prompt "switches to dirs prompt" "dirs>"
    assert_contains "shows sub directory" "sub"
    send "C-t"  # toggle back to files
    sleep 0.8
    assert_prompt "switches back to mixed prompt" "mixed>"
    send "C-g"
    sleep 0.3
}

test_git_status_display() {
    echo "# test_git_status_display"
    launch_fzfx
    wait_for "mixed>" 3 || true
    # README.md was modified (unstaged), should show U marker
    assert_contains "shows unstaged marker" "U"
    send "C-g"
    sleep 0.3
}

test_navigate_into_dir() {
    echo "# test_navigate_into_dir"
    launch_fzfx
    wait_for "mixed>" 3 || true
    send "C-t"  # switch to dirs
    sleep 0.8
    # Select "sub" and navigate into it
    send "C-u"
    sleep 0.2
    send "sub"
    sleep 0.5
    send "C-o"  # navigate into
    sleep 1
    # After navigating into sub, should show files in sub/
    assert_prompt "shows mixed prompt after navigate" "mixed>"
    assert_contains "shows nested dir content" "nested.txt"
    send "C-g"
    sleep 0.3
}

test_navigate_up() {
    echo "# test_navigate_into_then_up"
    launch_fzfx
    wait_for "mixed>" 3 || true
    send "C-t"  # dirs mode
    sleep 0.8
    send "C-u"
    sleep 0.2
    send "sub"
    sleep 0.5
    send "C-o"  # into sub
    sleep 1
    send "C-l"  # navigate up
    sleep 1
    assert_contains "back to root shows README" "README.md"
    send "C-g"
    sleep 0.3
}

test_swap_query_format() {
    echo "# test_swap_query_format"
    launch_fzfx
    wait_for "mixed>" 3 || true
    send "C-u"
    sleep 0.2
    send "#hello"
    sleep 0.8
    assert_prompt "in rg mode" "rg>"
    send "M-r"  # swap format
    sleep 0.8
    # After swap, #hello should become hello# (FzfRgPending)
    assert_prompt "swapped to fzf# prompt" "fzf#>"
    send "C-g"
    sleep 0.3
}

test_diff_preview_toggle() {
    echo "# test_diff_preview_toggle"
    launch_fzfx
    wait_for "mixed>" 3 || true
    send "M-g"  # toggle diff preview
    sleep 0.5
    # The header should update to show diff mode active
    assert_contains "diff mode shown in header" "diff"
    send "C-g"
    sleep 0.3
}

test_status_filter() {
    echo "# test_status_filter"
    launch_fzfx
    wait_for "mixed>" 3 || true
    send "M-u"  # filter to unstaged
    sleep 0.8
    # Should show only unstaged files (README.md is modified)
    assert_contains "shows unstaged README" "README.md"
    send "C-g"
    sleep 0.3
}

test_progressive_navigate_down_and_up() {
    echo "# test_progressive_navigate_down_and_up"
    launch_fzfx
    wait_for "mixed>" 3 || true

    # --- Level 0: project root ---
    assert_contains "L0: root has README" "README.md"
    assert_contains "L0: root has src/Main.hs" "Main.hs"

    # Switch to dirs mode
    send "C-t"
    sleep 0.8
    assert_prompt "L0: dirs prompt" "dirs>"
    assert_contains "L0: dirs shows sub" "sub"

    # --- Descend into sub/ ---
    send "C-u"
    sleep 0.2
    send "sub"
    sleep 0.5
    send "C-o"  # navigate into sub
    sleep 1.2
    wait_for "mixed>" 3 || true

    # Should now be in sub/, showing its files
    assert_contains "L1: sub has mid.txt" "mid.txt"

    # Switch to dirs to go deeper
    send "C-t"
    sleep 0.8
    send "C-u"  # clear any leftover query
    sleep 0.5
    assert_prompt "L1: dirs prompt" "dirs>"
    assert_contains "L1: dirs shows deep" "deep"

    # --- Descend into sub/deep/ ---
    send "deep"
    sleep 0.5
    send "C-o"  # navigate into deep
    sleep 1.2
    wait_for "mixed>" 3 || true

    assert_contains "L2: deep has nested.txt" "nested.txt"

    # Switch to dirs to go deeper
    send "C-t"
    sleep 0.8
    send "C-u"  # clear any leftover query
    sleep 0.5
    assert_contains "L2: dirs shows bottom" "bottom"

    # --- Descend into sub/deep/bottom/ ---
    send "bottom"
    sleep 0.5
    send "C-o"
    sleep 1.2
    wait_for "mixed>" 3 || true

    assert_contains "L3: bottom has floor.txt" "floor.txt"

    # --- Now ascend back up: C-l ---
    send "C-l"  # up from bottom → deep
    sleep 1.2
    wait_for "mixed>" 3 || true
    assert_contains "L2 back: deep has nested.txt" "nested.txt"

    send "C-l"  # up from deep → sub
    sleep 1.2
    wait_for "mixed>" 3 || true
    assert_contains "L1 back: sub has mid.txt" "mid.txt"

    send "C-l"  # up from sub → root
    sleep 1.2
    wait_for "mixed>" 3 || true
    assert_contains "L0 back: root has README" "README.md"
    assert_contains "L0 back: root has Main.hs" "Main.hs"

    send "C-g"
    sleep 0.3
}

test_navigate_alt_l_into_top() {
    echo "# test_navigate_alt_l_into_top"
    launch_fzfx
    wait_for "mixed>" 3 || true

    # Switch to dirs mode
    send "C-t"
    sleep 0.8

    # Select "sub" (which has sub/deep/bottom structure)
    # alt-l (into_top) should navigate into just the top-level dir component
    send "C-u"
    sleep 0.2
    send "sub"
    sleep 0.5
    send "M-l"  # alt-l = into_top
    sleep 1.2
    wait_for "mixed>" 3 || true

    # into_top stays in dirs mode — should show sub's subdirectories
    assert_prompt "into_top: dirs prompt" "dirs>"
    assert_contains "into_top: sub has deep dir" "deep"

    # Navigate back to root
    send "C-l"
    sleep 1.2
    assert_prompt "into_top back: dirs prompt" "dirs>"
    assert_contains "into_top back: root has sub dir" "sub"

    send "C-g"
    sleep 0.3
}

test_navigate_ctrl_r_to_root() {
    echo "# test_navigate_ctrl_r_to_root"
    launch_fzfx
    wait_for "mixed>" 3 || true

    # Navigate into sub/deep
    send "C-t"
    sleep 0.8
    send "C-u"
    sleep 0.2
    send "sub"
    sleep 0.5
    send "C-o"
    sleep 1.2
    send "C-t"
    sleep 0.8
    send "C-u"
    sleep 0.2
    send "deep"
    sleep 0.5
    send "C-o"
    sleep 1.2
    wait_for "mixed>" 3 || true
    assert_contains "in deep: has nested.txt" "nested.txt"

    # C-r should jump straight to git root
    send "C-r"
    sleep 1.2
    wait_for "mixed>" 3 || true
    assert_contains "ctrl-r: back at root with README" "README.md"
    assert_contains "ctrl-r: back at root with Main.hs" "Main.hs"

    send "C-g"
    sleep 0.3
}

# ═══════════════════════════════════════════════════════════════════════
# Runner
# ═══════════════════════════════════════════════════════════════════════

main() {
    if [[ ! -x "$FZFX" ]]; then
        echo "fzfx binary not found at $FZFX" >&2
        echo "Build first: nix build" >&2
        exit 1
    fi

    setup_test_dir
    echo "Test dir: $TEST_DIR"
    echo "Tmux socket: $TSOCK"
    echo ""

    test_basic_launch
    test_file_filtering
    test_rg_mode_switch
    test_rg_locked_mode
    test_toggle_hidden
    test_toggle_dirs_mode
    test_git_status_display
    test_navigate_into_dir
    test_navigate_up
    test_swap_query_format
    test_diff_preview_toggle
    test_status_filter
    test_progressive_navigate_down_and_up
    test_navigate_alt_l_into_top
    test_navigate_ctrl_r_to_root

    echo ""
    echo "$PASSED/$((PASSED + FAILED)) integration tests passed"
    if [[ ${#FAILURES[@]} -gt 0 ]]; then
        echo ""
        echo "Failures:"
        for f in "${FAILURES[@]}"; do
            echo "  - $f"
        done
        exit 1
    fi
}

main
