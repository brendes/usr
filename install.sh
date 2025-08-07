#!/bin/sh

set -e

os=$(uname | tr '[:upper:]' '[:lower:]')
repo=$(cd -- "$(dirname -- "$0")" && pwd)
dot_dir=${repo}/etc
dotconfig_dir=${dot_dir}/config

dry_run=false
if [ "$1" = "--dry-run" ]; then
    dry_run=true
    echo "Dry run - No changes will be made"
fi

safe_ln() {
    src="$1"
    dest="$2"
    if [ ! -e "$src" ]; then
        echo "Warning: source does not exist: $src"
        return 1
    fi
    if $dry_run; then
        echo "Would link: $dest -> $src"
        return 0
    fi
    if [ -f "$dest" ] && [ ! -L "$dest" ]; then
        mv "$dest" "$dest.bak"
        echo "Backed up: $dest -> $dest.bak"
    fi
    ln -sf "$src" "$dest"
    echo "Linked: $dest -> $src"
}

install_dots() {
    echo "Installing dotfiles..."
    dest_dir=${HOME}
    for f in \
        Xresources \
        bash_profile \
        bashrc \
        exrc \
        lynxrc \
        profile \
        rc \
        sword \
        tmux.conf \
        vim \
        zshenv \
        zshrc
    do
        safe_ln "${dot_dir}/${f}" "${dest_dir}/.${f}"
    done
}

install_dotconfig() {
    echo "Installing config files..."
    dest_dir=${HOME}/.config
    if ! $dry_run; then
        mkdir -p "$dest_dir"
    fi
    for f in \
        cmus \
        emacs \
        ghostty \
        git \
        kitty \
        lynx \
        newsboat \
        nvim \
        rstudio \
        wezterm \
        zathura \
        zed
    do
        safe_ln "${dotconfig_dir}/${f}" "${dest_dir}/${f}"
    done
}

install_vscode() {
    echo "Installing VSCode settings..."
    dest_dir="${HOME}/Library/Application Support/Code/User"
    if ! $dry_run; then
        mkdir -p "$dest_dir"
    fi
    safe_ln "${dot_dir}/vscode/keybindings.json" "${dest_dir}/keybindings.json"
}

install_termux() {
    if [ -n "$TERMUX_VERSION" ]; then
        echo "Installing Termux config..."
        safe_ln "${dot_dir}/termux" "${HOME}/.termux"
    fi
}

install_openbsd() {
    echo "Installing OpenBSD config..."
    safe_ln "${dot_dir}/kshrc" "${HOME}/.kshrc"
}

install_dots
install_dotconfig
install_termux

case $os in
    darwin) install_vscode ;;
    openbsd) install_openbsd ;;
esac

if $dry_run; then
    echo "Dry run completed"
else
    echo "Installation completed"
fi
