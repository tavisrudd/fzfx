# fzfx smart bash completion
# Dispatches to fzfx for file/dir completion, fzf_bash_completion for commands/flags.
#
# Requires: fzfx on PATH, fzf-bash-completion.sh already sourced.
#
# Usage (add to ~/.bashrc after sourcing fzf-bash-completion.sh):
#   source /path/to/fzfx/shell/bash-completion.bash

_fzfx_env_var_matches() {
    local prefix="$1"
    if type compgen >/dev/null 2>&1; then
        compgen -A variable -- "$prefix"
        return
    fi

    local env_name
    while IFS='=' read -r env_name _; do
        [[ "$env_name" == "$prefix"* ]] && printf '%s\n' "$env_name"
    done < <(env)
}

_fzfx_env_path_context() {
    local var_expr="$1"
    local env_value="$2"
    local env_rest="$3"
    local rest="${env_rest#/}"
    local dir_part=""

    fzfx_cwd="$env_value"
    query="$rest"
    replacement_prefix="${var_expr}/"

    [[ -z "$rest" ]] && return

    if [[ "$rest" == */ ]]; then
        dir_part="${rest%/}"
        query=""
    elif [[ "$rest" == */* ]]; then
        dir_part="${rest%/*}"
        query="${rest##*/}"
    else
        return
    fi

    if [[ -n "$dir_part" && -d "$env_value/$dir_part" ]]; then
        fzfx_cwd="$env_value/$dir_part"
        replacement_prefix="${var_expr}/${dir_part}/"
    fi
}

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

    local query="$cur"
    local fzfx_cwd=""
    local replacement_prefix=""

    # Bare variable prefixes should complete variable names, not file paths.
    if [[ "$cur" == '$' || "$cur" == '${' ]]; then
        fzf_bash_completion
        return
    fi

    # Environment-backed paths: $HOME/src or ${XDG_CONFIG_HOME}/nvim
    # should search from the expanded directory while preserving the
    # variable expression inserted back into the readline buffer.
    if [[ "$cur" =~ ^\$\{([A-Za-z_][A-Za-z0-9_]*)\}(/.*)?$ ]]; then
        local env_name="${BASH_REMATCH[1]}"
        local env_rest="${BASH_REMATCH[2]}"
        local env_value="${!env_name-}"
        if [[ -n "$env_value" && -d "$env_value" ]]; then
            _fzfx_env_path_context "\${${env_name}}" "$env_value" "$env_rest"
        fi
    elif [[ "$cur" =~ ^\$([A-Za-z_][A-Za-z0-9_]*)(/.*)?$ ]]; then
        local env_name="${BASH_REMATCH[1]}"
        local env_rest="${BASH_REMATCH[2]}"
        local env_value="${!env_name-}"
        if [[ -n "$env_value" && -d "$env_value" ]]; then
            _fzfx_env_path_context "\$${env_name}" "$env_value" "$env_rest"
        elif [[ -z "$env_rest" ]]; then
            local env_matches=()
            local env_match
            while IFS= read -r env_match; do
                env_matches+=("$env_match")
            done < <(_fzfx_env_var_matches "$env_name")
            if [[ ${#env_matches[@]} -eq 1 ]]; then
                local before="${READLINE_LINE:0:READLINE_POINT}"
                local after="${READLINE_LINE:READLINE_POINT}"
                before="${before%"$cur"}"
                READLINE_LINE="${before}\$${env_matches[0]}${after}"
                READLINE_POINT=$(( ${#before} + ${#env_matches[0]} + 1 ))
                return
            fi
            fzf_bash_completion
            return
        fi
    elif [[ "$cur" =~ ^\$\{([A-Za-z_][A-Za-z0-9_]*)$ ]]; then
        local env_name="${BASH_REMATCH[1]}"
        local env_matches=()
        local env_match
        while IFS= read -r env_match; do
            env_matches+=("$env_match")
        done < <(_fzfx_env_var_matches "$env_name")
        if [[ ${#env_matches[@]} -eq 1 ]]; then
            local before="${READLINE_LINE:0:READLINE_POINT}"
            local after="${READLINE_LINE:READLINE_POINT}"
            before="${before%"$cur"}"
            READLINE_LINE="${before}\${${env_matches[0]}}${after}"
            READLINE_POINT=$(( ${#before} + ${#env_matches[0]} + 3 ))
            return
        fi
        fzf_bash_completion
        return
    fi

    # Everything else: file/dir picking via fzfx
    local cmd="${stripped%%[[:space:]]*}"
    local fzfx_args=(-o stdout --hidden)
    if [[ -n "$fzfx_cwd" ]]; then
        fzfx_args+=(--cwd "$fzfx_cwd")
    fi

    # Directory-only mode for commands that expect directories, mixed for general use
    case "$cmd" in
        cd|pushd|rmdir|mkdir|z) fzfx_args+=(-t d) ;;
        *) fzfx_args+=(-t m --height=auto) ;;
    esac

    # Show current command line as the fzf prompt
    fzfx_args+=(--prompt="> ${line} ")

    # Pass partial word as initial query
    if [[ -n "$query" ]]; then
        fzfx_args+=("$query")
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
        quoted_selection+="${quoted_selection:+ }${replacement_prefix}${sel}"
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
