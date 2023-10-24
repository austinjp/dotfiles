# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "${HOME}/.bashrc" ]; then
	. "${HOME}/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/bin" ] ; then
    export PATH="${HOME}/bin:${PATH}"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/.local/bin" ] ; then
    export PATH="${HOME}/.local/bin:${PATH}"
fi

if [ -f "${HOME}/.cargo/env" ]; then
    . "${HOME}/.cargo/env"
fi

# Update timezone. Added 2023-03-20 (but see update below)
# to try fixing random read-only disk issues!
# 
# TIMTEZONE/TZ settings. If changing this:
#   1. Edit this file.
#   2. Start a shell, and check $TZ
#   3. Run timedatectl set-timezone "${TZ}"
#   4. Check/edit /etc/ntp.conf
#   5. Probably reboot!
# For valid values of TZ, run tzelect.
# 
# Updated 2023-03-31
# 
# To set timezone for whole box:
#   sudo dpkg-reconfigure tzdata
# TZ='Europe/London'
# export TZ
