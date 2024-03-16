#!/usr/bin/env /bin/bash
# -*- mode: shell-script; sh-shell: bash; sh-basic-offset: 4; sh-indentation: 4; coding: utf-8 -*-
# shellcheck shell=bash

set -o nounset -o errexit -o errtrace -o pipefail

updated="false"

function defaults_write_if_needed() {
   local -r domain="${1}"
   local -r key="${2}"
   local -r type="${3}"
   local -r value="${4}"

   local current
   local unset="false"

   if ! current="$(defaults read "${domain}" "${key}" 2> /dev/null)"; then
       unset="true"
   fi

   if [[ "${type}" = "-bool" ]]; then
       if [[ "${current}" = "0" ]]; then
           current="false"
       elif [[ "${current}" = "1" ]]; then
           current="true"
       fi
   fi

   if [[ "${type}" = "-array" ]]; then
       local empty_array="(
)"
       if [[ "${current}" = "${empty_array}" ]]; then
           current=""
       fi
   fi

   if [[ "${unset}" = "true" ]] || [[ "${current}" != "${value}" ]]; then
       echo >&2 "Update needed, executing 'defaults write ${domain} ${key} ${type} ${value}'."

       defaults write "${domain}" "${key}" "${type}" "${value}"
       updated="true"
   fi
}

function setup_dock() {
    updated="false"

    # Enable autohiding of Dock
    defaults_write_if_needed com.apple.dock autohide -bool true
    # Set Dock position to left
    defaults_write_if_needed com.apple.dock orientation -string "left"
    # Only show currently running apps
    defaults_write_if_needed com.apple.dock persistent-apps -array ""
    defaults_write_if_needed com.apple.dock static-only -bool true
    # Group windows by application in Mission Control: https://macos-defaults.com/mission-control/expose-group-apps.html
    defaults_write_if_needed com.apple.dock expose-group-apps -bool true
    # Disable auto arranging spaces based on MRU
    defaults_write_if_needed com.apple.dock mru-spaces -bool false

    # After Dock configuration, restart
    if [[ "${updated}" = "true" ]]; then
        echo >&2 "Updated Dock configuration, restarting dock."
        killall Dock
    fi
}

desired_sidebar="Recents -> file:///System/Library/CoreServices/Finder.app/Contents/Resources/MyLibraries/myDocuments.cannedSearch/
Applications -> file:///Applications/
Home -> file:///Users/travisbhartwell/
Desktop -> file:///Users/travisbhartwell/Desktop/
Developer -> file:///Users/travisbhartwell/Developer/
Documents -> file:///Users/travisbhartwell/Documents/
Downloads -> file:///Users/travisbhartwell/Downloads/"

# Configure Finder Sidebar: https://github.com/mosen/mysides
function setup_finder_sidebar() {
    local current

    current="$(mysides list)"

    if [[ "${current}" != "${desired_sidebar}" ]]; then
        echo >&2 "Recreating sidebar"

        mysides remove all
        mysides add Recents file:///System/Library/CoreServices/Finder.app/Contents/Resources/MyLibraries/myDocuments.cannedSearch/
        mysides add Applications file:///Applications
        mysides add Home "file://${HOME}"
        mysides add Desktop "file://${HOME}/Desktop/"
        mysides add Developer "file://${HOME}/Developer/"
        mysides add Documents "file://${HOME}/Documents/"
        mysides add Downloads "file://${HOME}/Downloads/"
    else
        echo >&2 "Sidebar correctly configured"
    fi
}

function setup_finder() {
    updated="false"

    # Hide all icons on the desktop: https://macos-defaults.com/desktop/createdesktop.html
    defaults_write_if_needed com.apple.finder CreateDesktop -bool false

    # Show file extensions: https://macos-defaults.com/finder/appleshowallextensions.html
    defaults_write_if_needed NSGlobalDomain AppleShowAllExtensions -bool true

    # Show path bar: https://macos-defaults.com/finder/showpathbar.html
    defaults_write_if_needed com.apple.finder ShowPathbar -bool true

    # Show status bar: https://git.herrbischoff.com/awesome-macos-command-line/about/#status-bar
    defaults_write_if_needed com.apple.finder ShowStatusBar -bool true

    # Sort folders on top: https://macos-defaults.com/finder/_fxsortfoldersfirst.html
    defaults_write_if_needed com.apple.finder _FXSortFoldersFirst -bool true

    # Home is default folder: https://git.herrbischoff.com/awesome-macos-command-line/about/#set-default-finder-location-to-home-folder
    defaults_write_if_needed com.apple.Finder NewWindowTarget -string PfLo
    defaults_write_if_needed com.apple.Finder NewWindowTargetPath -string "file://${HOME}"

    # Expand Save Panel by Default: https://git.herrbischoff.com/awesome-macos-command-line/about/#expand-save-panel-by-default
    defaults_write_if_needed NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
    defaults_write_if_needed NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

    # Hide Recent Tags in Sidebar: https://derflounder.wordpress.com/2021/11/29/disabling-recent-tags-in-the-finder-window-sidebar/
    defaults_write_if_needed com.apple.Finder ShowRecentTags -bool false

    # Don't hide ~/Library: https://git.herrbischoff.com/awesome-macos-command-line/about/#toggle-folder-visibility-in-finder
    chflags nohidden ~/Library

    setup_finder_sidebar

    # After Finder configuration, restart
    if [[ "${updated}" = "true" ]]; then
        echo >&2 "Updated Finder configuration, restarting Finder"
        killall Finder
    fi
}

function setup_screenshots() {
    local -r screenshots_dir="${HOME}/Pictures/Screenshots"

    if [[ ! -d "${screenshots_dir}" ]]; then
        echo >&2 "Creating missing screenshot directory: '${screenshots_dir}'."
        mkdir -p "${screenshots_dir}"
    fi

    # https://macos-defaults.com/screenshots/location.html
    defaults_write_if_needed com.apple.screencapture "location" -string "${screenshots_dir}"
}

function setup_misc() {
    # Disable autocorrect: https://git.herrbischoff.com/awesome-macos-command-line/about/#auto-correct
    defaults_write_if_needed NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

    # Enable tab in modal dialogs: https://git.herrbischoff.com/awesome-macos-command-line/about/#full-keyboard-access
    defaults_write_if_needed NSGlobalDomain AppleKeyboardUIMode -int 3
}

# https://azimi.io/how-to-enable-touch-id-for-sudo-on-macbook-pro-46272ac3e2df
function setup_sudo_touchid() {
    local -r local_config="/etc/pam.d/sudo_local"
    local create_needed="true"

    if [[ -e "${local_config}" ]]; then
        if grep -q "^auth       sufficient     pam_tid.so$" "${local_config}"; then
            create_needed="false"
        fi
    fi

    if [[ "${create_needed}" = "true" ]]; then
        cat <<EOF | sudo tee /etc/pam.d/sudo_local
# sudo_local: local config file which survives system update and is included for sudo
# uncomment following line to enable Touch ID for sudo
auth       sufficient     pam_tid.so
EOF
    fi
}


setup_dock
setup_finder
setup_screenshots
setup_misc
setup_sudo_touchid
