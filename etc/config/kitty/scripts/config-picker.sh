#!/bin/sh
set -euo pipefail

kitty_home="${HOME}/.config/kitty"
dir="${kitty_home}/$1"
current_link="${dir}/current"

# Try various methods of reloading all kitty instances
reload_kitty() {
    if [ -n "${KITTY_PID-}" ] && kill -0 "$KITTY_PID" 2>/dev/null; then
        kill -USR1 "$KITTY_PID"
        return
    fi

    pkill -USR1 -x kitty 2>/dev/null || true
}

# Remember existing target in case user aborts
[ -L "$current_link" ] && original_target=$(readlink "$current_link") || original_target=""

cd "$dir" || exit 1

picked=$(
  find . -maxdepth 1 -type f -name '*.conf' \
    | sed 's|^\./||' \
    | sort -r \
    | fzf --no-sort --no-color --style=minimal --tmux 30%
)

if [ -n "$picked" ]; then
    ln -sfn "$dir/$picked" "$current_link"
else
    # Restore original link if selection cancelled
    [ -n "$original_target" ] && ln -sfn "$original_target" "$current_link"
fi

reload_kitty
