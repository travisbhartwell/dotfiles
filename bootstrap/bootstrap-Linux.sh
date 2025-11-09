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

function error_msg() {
    printf '%b\n' "$*" >&2
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

    ssh-keygen -t ed25519 -C "nafai@travishartwell.net" -f "${SSH_KEY_FILE}"
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

    ssh-add "${SSH_KEY_FILE}"
}

function install_minimum_packages() {
    # Install the absolute minimum packages needed to bootstrap everything
    echo_msg "Installing minimum packages from Ubuntu"
    sudo apt update \
        && sudo apt upgrade \
        && sudo apt install -y git gh zsh gpg sudo wget curl
}

function install_mise() {
    # https://mise.jdx.dev/getting-started.html#installing-mise-cli
    echo_msg "Installing Mise via the apt repository"

    if dpkg -s mise &>/dev/null; then
        echo_msg "Mise is already installed"
        return 0
    fi

    sudo install -dm 755 /etc/apt/keyrings
    wget -qO - https://mise.jdx.dev/gpg-key.pub | gpg --dearmor | sudo tee /etc/apt/keyrings/mise-archive-keyring.gpg 1>/dev/null
    echo "deb [signed-by=/etc/apt/keyrings/mise-archive-keyring.gpg arch=amd64] https://mise.jdx.dev/deb stable main" | sudo tee /etc/apt/sources.list.d/mise.list
    sudo apt update && sudo apt install -y mise
}

function configure_github() {
    if ! command -v gh >/dev/null 2>&1; then
        echo_msg "Missing required 'gh'."
        return 1
    fi

    # https://stackoverflow.com/a/78890003
    if [[ -n "${GH_AUTH_HEADLESS-}" ]]; then
        echo_msg "Configuring 'gh' for github access, open 'https://github.com/login/device' locally and paste the one-time code printed."
        BROWSER=false gh auth login --git-protocol ssh
    else
        echo_msg "Configuring 'gh' for github access, will require authentication in browser."
        gh auth login --web --git-protocol ssh
    fi
}

function install_and_initialize_chezmoi() {
    if ! command -v mise >/dev/null 2>&1; then
        echo_msg "Missing required 'mise'."
        return 1
    fi

    mise use --global chezmoi
    mise exec -- chezmoi init --ssh --apply travisbhartwell
}

function detect_linux_version() {
    if [[ "$(uname -s || true)" != "Linux" ]]; then
        error_msg "This script only supports Linux."
        return 1
    fi

    local distro
    local architecture

    # TODO: Add detection of other Linux versions
    if [[ -e /etc/os-release ]]; then
        if grep -q '^NAME="Ubuntu"$' /etc/os-release; then
            distro="Ubuntu"
        else
            error_msg "Currently this script only supports Ubuntu."
            return 1
        fi
    fi
    readonly distro

    architecture="$(uname -m)"

    if [[ "${architecture}" != "x86_64" ]]; then
        error_msg "Currently this script only supports the x86_64 architecture."
        return 1
    fi

    echo "${distro}:${architecture}"
}

function configure_login_shell() {
    local zsh_path
    if ! zsh_path="$(which zsh)"; then
        error_msg "Zsh is not installed, not configuring login shell."
        return 1
    fi
    readonly zsh_path

    echo_msg "Configuring the login shell for ${USER} to ${zsh_path}"

    if ! chsh -s "${zsh_path}"; then
        error_msg "Error setting user shell to Zsh"
        return 1
    fi
}

function main() {
    echo_msg "Bootstrapping Configuration"

    local distro_arch
    if ! distro_arch="$(detect_linux_version)"; then
        exit 1
    fi
    readonly distro_arch

    echo_msg "Performing setup for '${distro_arch}'."

    echo_msg "Authenticating with sudo"
    sudo -v

    while true; do
        sudo -n true
        sleep 60
        kill -0 "$$" || exit
    done 2>/dev/null &

    install_minimum_packages
    install_mise

    echo_msg "Starting GitHub SSH Key Configuration"
    generate_key
    configure_ssh
    start_ssh_agent
    add_key_to_agent
    configure_github
    echo_msg "Completed GitHub SSH Key Configuration"

    echo_msg "Starting Chezmoi Install and Initialization"
    install_and_initialize_chezmoi
    echo_msg "completed Chezmoi Install and Initialization"

    echo_msg "Configuring zsh as the user shell"
    configure_login_shell
    echo_msg "Completed configuring zsh as the user shell"
}

main "${@}"
