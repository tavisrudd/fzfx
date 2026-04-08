# fzfx smart bash completion
# Dispatches to fzfx for file/dir completion, fzf_bash_completion for commands/flags.
#
# Requires: fzfx on PATH, fzf-bash-completion.sh already sourced.
#
# Usage (add to ~/.bashrc after sourcing fzf-bash-completion.sh):
#   source /path/to/fzfx/shell/bash-completion.bash

_fzfx_smart_completion() {
    local line="${READLINE_LINE:0:READLINE_POINT}"

    # Get the last simple command (after the last pipe/semicolon/&&/||)
    local cmd_line="$line"
    local sep
    for sep in '|' ';' '&&' '||'; do
        cmd_line="${cmd_line##*$sep}"
    done

    # Strip leading whitespace
    local stripped="${cmd_line#"${cmd_line%%[![:space:]]*}"}"

    # Completing a command name (empty line or first word) -> fzf_bash_completion
    if [[ -z "$stripped" || ! "$stripped" =~ [[:space:]] ]]; then
        fzf_bash_completion
        return
    fi

    # Get the current (partial) word at cursor
    local cur="${stripped##* }"

    # Flags/options -> fzf_bash_completion
    if [[ "$cur" == -* ]]; then
        fzf_bash_completion
        return
    fi

    # Everything else: file/dir picking via fzfx
    local cmd="${stripped%%[[:space:]]*}"
    local fzfx_args=(-o stdout --hidden)

    # Directory-only mode for commands that expect directories, mixed for general use
    case "$cmd" in
        cd|pushd|rmdir|mkdir|z) fzfx_args+=(-t d) ;;
        *) fzfx_args+=(-t m --height=auto) ;;
    esac

    # Show current command line as the fzf prompt
    fzfx_args+=(--prompt="> ${line} ")

    # Pass partial word as initial query
    if [[ -n "$cur" ]]; then
        fzfx_args+=("$cur")
    fi

    printf '\r'

    local selection quoted_selection="" sel
    selection="$(command fzfx "${fzfx_args[@]}")" || { printf '\r'; command tput el 2>/dev/null || echo -ne "\033[K"; return; }
    [[ -z "$selection" ]] && { printf '\r'; command tput el 2>/dev/null || echo -ne "\033[K"; return; }

    # Quote each selected path if needed, join with spaces
    while IFS= read -r sel; do
        [[ -z "$sel" ]] && continue
        # Strip rg-style :line:col suffix
        if [[ "$sel" =~ ^(.+):[0-9]+:[0-9]+$ ]]; then
            sel="${BASH_REMATCH[1]}"
        elif [[ "$sel" =~ ^(.+):[0-9]+$ ]]; then
            sel="${BASH_REMATCH[1]}"
        fi
        if [[ "$sel" =~ [[:space:]\'\"\;\&\|\<\>\(\)\$\`\\\#\~\*\?] ]]; then
            printf -v sel '%q' "$sel"
        fi
        quoted_selection+="${quoted_selection:+ }${sel}"
    done <<< "$selection"

    # Splice into the readline buffer, replacing any partial word
    local before="${READLINE_LINE:0:READLINE_POINT}"
    local after="${READLINE_LINE:READLINE_POINT}"
    if [[ -n "$cur" ]]; then
        before="${before%"$cur"}"
    fi
    READLINE_LINE="${before}${quoted_selection}${after}"
    READLINE_POINT=$(( ${#before} + ${#quoted_selection} ))

    printf '\r'
    command tput el 2>/dev/null || echo -ne "\033[K"
}

bind -m emacs -x '"\e/": _fzfx_smart_completion'
