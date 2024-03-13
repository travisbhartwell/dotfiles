#!/usr/bin/env bash
# -*- mode: shell-script; sh-shell: bash; sh-basic-offset: 4; sh-indentation: 4; coding: utf-8 -*-
# shellcheck shell=bash

set -o nounset -o errexit -o errtrace -o pipefail

readonly SSH_DIR="${HOME}/.ssh"
readonly SSH_KEY_FILE="${SSH_DIR}/id_ed25519-github"
readonly SSH_CONFIG="${SSH_DIR}/config"

function echo_msg() {
    printf '%b\n' "$*"
}

function generate_key() {
    if [[ -e "${SSH_KEY_FILE}" ]]; then
        echo_msg "SSH Key already exists"
        return
    fi

    echo_msg "Generating SSH Key"

    if [[ ! -d "${SSH_DIR}" ]]; then
        mkdir -p "${SSH_DIR}"
    fi

    ssh-keygen -t ed25519 -C "nafai@travishartwell.net" -f "${SSH_DIR}"
}

function configure_ssh() {
    if [[ -e "${SSH_CONFIG}" ]]; then
        if grep -q "Host github.com" "${SSH_CONFIG}"; then
            echo_msg "SSH Already Configured"
            return
        fi
    fi

    echo_msg "Configuring SSH"

    cat <<EOF >>"${SSH_CONFIG}"
Host github.com
  AddKeysToAgent yes
  UseKeychain yes
  IdentityFile ~/.ssh/id_ed25519-github

EOF
}

function start_ssh_agent() {
    if ! pgrep ssh-agent &>/dev/null; then
        echo_msg "Starting ssh-agent"
        eval "$(ssh-agent -s || true)"
    fi
}

function add_key_to_agent() {
    echo_msg "Adding key to agent"

    ssh-add --apple-use-keychain "${SSH_KEY_FILE}"
}

function update_mac_os() {
    sudo softwareupdate --install --all --verbose
}

function install_rosetta() {
    if pkgutil --pkg-info=com.apple.pkg.RosettaUpdateAuto >/dev/null 2>&1; then
        echo_msg "Rosetta is already installed"
    else
        echo_msg "Installing Rosetta"
        softwareupdate --install-rosetta --agree-to-license
    fi
}

function install_homebrew() {
    if command -v brew >/dev/null 2>&1; then
        echo_msg "Homebrew is already installed"
        return
    fi

    echo_msg "Installing Homebrew"

    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh || true)"
}

function install_packages() {
    # Install the absolute minimum packages needed to bootstrap everything
    echo_msg "Installing minimum packages from Homebrew"
    brew install gh
    brew install chezmoi
}

function configure_github() {
    if ! command -v gh >/dev/null 2>&1; then
        echo_msg "Missing required 'gh'."
        return 1
    fi

    echo_msg "Configuring 'gh' for github access, will require authentication in browser."
    gh auth login --web --git-protocol ssh
}

function initialize_chezmoi() {
    if ! command -v chezmoi >/dev/null 2>&1; then
        echo_msg "Missing required 'chezmoi'."
        return 1
    fi

    chezmoi init --apply travisbhartwell
}

function main() {
    echo_msg "Bootstrapping Configuration"

    echo_msg "Authenticating with sudo"
    sudo -v

    while true; do
        sudo -n true
        sleep 60
        kill -0 "$$" || exit
    done 2>/dev/null &

    echo_msg "Starting to update Mac OS and installing prerequsites"
    update_mac_os
    install_rosetta
    echo_msg "Completed updating Mac OS and installing prerequsites"

    echo_msg "Starting installing and configuring Homebrew"
    install_homebrew

    if [[ -x /opt/homebrew/bin/brew ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv || true)"
    else
        echo_msg "Homebrew install failed"
        exit 1
    fi
    install_packages
    echo_msg "Completed installing and configuring Homebrew"

    echo_msg "Starting GitHub SSH Key Configuration"
    generate_key
    configure_ssh
    start_ssh_agent
    add_key_to_agent
    configure_github
    echo_msg "Completed GitHub SSH Key Configuration"

    echo_msg "Starting Chezmoi Initialization"
    initialize_chezmoi
    echo_msg "completed Chezmoi Initialization"
}

main "${@}"
