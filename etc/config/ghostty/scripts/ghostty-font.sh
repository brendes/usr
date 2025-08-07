#!/bin/sh

dir="$HOME/.config/ghostty/fonts"
original_link=""

if [ -L "${dir}/current" ]; then
    original_link=$(readlink "${dir}/current")
fi

cd "${dir}" || exit 1

selected=$(
  find . -maxdepth 1 -type f \
    | grep -v current \
    | grep -v colors-light \
    | sort -r \
    | fzf --no-sort --no-color --style=minimal
)

if [ -n "$selected" ]; then
    ln -sfn "$selected" "${dir}/current"
else
    if [ -n "$original_link" ]; then
        ln -sfn "$original_link" "${dir}/current"
    fi
fi
