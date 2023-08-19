#!/usr/bin/env bash
# -*- mode: shell-script; sh-shell: bash; sh-basic-offset: 4; sh-indentation: 4; coding: utf-8 -*-
# shellcheck shell=bash

# This is going to be the simplest possible thing that could work
# until the full version is in MyCmd itself.

set -o nounset -o errexit -o errtrace -o pipefail

if ! PROJECT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P); then
    echo >&2 "Error fetching project directory."
    exit 1
fi

readonly MYCMD_DIR="${PROJECT_DIR}/mycmd"

function all-bash-files-breadth-first() {
    # shellcheck disable=SC2312
    readarray -t ALL_BASH_FILES < <((
        echo -e "0\t${PROJECT_DIR}/project.sh"
        gfind "${MYCMD_DIR}" -type f -printf '%d\t%p\n'
    ) | sort -nk1 | cut -f2- | xargs grealpath --relative-to="${PROJECT_DIR}")
}

declare -ax ALL_BASH_FILES=()
all-bash-files-breadth-first
readonly ALL_BASH_FILES

function all-command-groups-breadth-first() {
    # shellcheck disable=SC2312
    readarray -t ALL_COMMAND_GROUPS < <(
        gfind "${MYCMD_DIR}" -name "*-lib" -type f -printf '%d\t%p\n' \
            | sort -nk1 \
            | cut -f2- \
            | xargs grealpath --relative-to="${PROJECT_DIR}"
    )
}

declare -ax ALL_COMMAND_GROUPS=()
all-command-groups-breadth-first
readonly ALL_COMMAND_GROUPS

function all-commands-breadth-first() {
    # shellcheck disable=SC2312
    readarray -t ALL_COMMANDS < <(
        gfind "${MYCMD_DIR}" -type f ! -name "*-lib" -printf '%d\t%p\n' \
            | sort -nk1 \
            | cut -f2- \
            | xargs grealpath --relative-to="${PROJECT_DIR}"
    )
}

declare -ax ALL_COMMANDS=()
all-commands-breadth-first
readonly ALL_COMMANDS

function list-files() {
    echo "${*}" | tr ' ' '\n'
}

function list-all-bash-scripts() {
    list-files "${ALL_BASH_FILES[@]}"
}

function list-all-command-groups() {
    list-files "${ALL_COMMAND_GROUPS[@]}"
}

function list-all-commands() {
    list-files "${ALL_COMMANDS[@]}"
}

function format-bash-scripts() {
    echo "Formatting the following files:"
    list-files "${@}"

    cd "${PROJECT_DIR}"
    shfmt --language-dialect bash --indent=4 --binary-next-line --case-indent --write "${@}"
}

function format-all-bash-scripts() {
    if (("${#ALL_BASH_FILES[@]}" == 0)); then
        echo >&2 "No files defined, skipping format."
        return 0
    fi

    format-bash-scripts "${ALL_BASH_FILES[@]}"
}

function lint-bash-scripts() {
    echo "Linting the following files:"
    list-files "${@}"

    cd "${PROJECT_DIR}"
    echo "Running ShellCheck:"
    shellcheck --check-sourced "${@}" && echo "Success"
}

function lint-all-bash-scripts() {
    if (("${#ALL_BASH_FILES[@]}" == 0)); then
        echo >&2 "No files defined, skipping lint check."
        return 0
    fi

    lint-bash-scripts "${ALL_BASH_FILES[@]}"
}

function function_exists() {
    declare -F "$1" >/dev/null
}

function call_if_function_exists() {
    local -r fn=$1
    shift

    if function_exists "${fn}"; then
        "${fn}" "$@"
    else
        echo >&2 "Unknown task: '${fn}'."
    fi
}

if (($# == 0)); then
    echo >&2 "Expecting task to run:"
    echo >&2 "$0 <task>"
    exit 1
fi

call_if_function_exists "${@}"
