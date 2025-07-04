# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# # If not running interactively, don't do anything
# case $- in
#     *i*) ;;
#       *) return;;
# esac


# ==============================================================================

# Added 2023-10-14 to "fix" error messages after uninstalling a thing.
# unset LD_PRELOAD


# ==============================================================================

# Don't add duplicate lines to the history.

# See bash(1) for more options
export HISTCONTROL=ignoredups

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=1000
export HISTFILESIZE=2000


# ==============================================================================

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar


# ==============================================================================

# Pagers. Defined early so aliases etc can use these env vars.

export PAGER='less -XFRi'

# Added 2022-04-18 for delta git pager.
export DELTA_PAGER="less -XFRS"

# ==============================================================================

# Updated 2024-06-05
# Add go bins to PATH without setting GOPATH or GOROOT which should
# apparently remain unset: https://stackoverflow.com/a/68226616
PATH="${PATH}:/usr/local/go/bin:${HOME}/go/bin"

# ==============================================================================
# Prompt stuff.

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

force_color_prompt=yes
if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

function bottom_prompt {
    tput cup $(($LINES-1)) 0
}

unset PS1

# if [ "$color_prompt" = yes ]; then
#     PS1='${debian_chroot:+($debian_chroot)}\[[01;32m\]\u@\h\[[00m\]:\[[01;34m\]\w\[[00m\]\$ '
# else
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
# fi

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS=1"\[]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\]$PS1"
    ;;
*)
    ;;
esac

# Added 2022-09-12 for powerline-go
# See https://github.com/justjanne/powerline-go
function _update_ps1_powerline() {
    PS1=$(powerline-go -error "${?}" -modules venv,user,host,ssh,cwd,perms,git,hg,jobs,exit -condensed -git-mode compact -cwd-max-depth 4 -cwd-mode semifancy -hostname-only-if-ssh -truncate-segment-width 20 -newline)

    # Uncomment the following line to automatically clear errors after showing
    # them once. This not only clears the error for powerline-go, but also for
    # everything else you run in that shell. Don't enable this if you're not
    # sure this is what you want.
    # set "?"
}
function _update_ps1() {
    PS1="\[\e[0;95m\]"$(pwd | sed -re 's,^'${HOME}',\~,g')"\[\e[0m\]"$'\n'"\[\e[0;36m\]"'$ '"\[\e[0m\]"
    # PS1=$(
    #     echo '\033[0;32;1m' $(pwd | sed -re 's,^'${HOME}',\~,g') '\033[0m' ;
    #     echo ;
    #     # echo -n $(pwd | sed -re 's,^'${HOME}',\~,g') ;
    #     # echo -e '\033[0m' ;
    #     # echo -e '\033[0;36;1m\$ \033[0m' ;
    #     # echo
    #     # echo -ne '\033[0m'$'\n\033[0;36;1m \$ \033[0m')"
    #    )
}
if [ "$TERM" != "linux" ] && [[ $(which powerline-go) ]]; then
    PROMPT_COMMAND="_update_ps1_powerline; $PROMPT_COMMAND"
else
    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
fi

# Workaround for nix-shell --pure
if [ "$IN_NIX_SHELL" == "pure" ]; then
    if [ -x "${HOME}/.nix-profile/bin/powerline-go" ]; then
        alias powerline-go="${HOME}/.nix-profile/bin/powerline-go"
    elif [ -x "/run/current-system/sw/bin/powerline-go" ]; then
        alias powerline-go="/run/current-system/sw/bin/powerline-go"
    fi
fi

export PS1

# ==============================================================================

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'


# ==============================================================================

# Added 2024-03-30 for flatpak.
export XDG_DATA_DIRS="${XDG_DATA_DIRS}:/var/lib/flatpak/exports/share:${HOME}/.local/share/flatpak/exports/share"

# ==============================================================================

# Alias definitions.
unalias -a
[[ -f ~/.bash_aliases ]] && . ~/.bash_aliases

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


# ==============================================================================

# Added 2021-08-11
# source ~/.bashrc.oh-my-bash

# This is already getting executed, probably via oh-my-bash.
# Enable programmable completion features.
# Must run after aliases to apply completion to aliases.
# Also see /etc/bash.bashrc and /etc/profile
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi


# ==============================================================================

# Added 2019-09-12: use specific ssh key in Git:
# export GIT_SSH_COMMAND="ssh -i ~/.ssh/id_bitbucket"
# Updated 2020-04-10
# Using ~/.ssh/config file instead.
# The following line ensures ssh-add can be used to
# add keys to gnome-keyring, the GUI key auth thing.

# Removed 2025-03-26 because this is provided by gnome-keyring-daemon below.
# export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/keyring/ssh"


# ==============================================================================

export $(gnome-keyring-daemon --daemonize --start 2>/dev/null)


# ==============================================================================

PATH="${PATH}:${HOME}/bin/"


# ==============================================================================

# Added 2020-01-08 for Android Studio
# as per https://facebook.github.io/react-native/docs/getting-started
# Changed 2024-03-01
export ANDROID_HOME="${HOME}/Android/Sdk"
# PATH="${PATH}:$ANDROID_HOME/tools"
# PATH="${PATH}:$ANDROID_HOME/tools/bin"
PATH="${PATH}:$ANDROID_HOME/bin"
PATH="${PATH}:$ANDROID_HOME/emulator"
PATH="${PATH}:$ANDROID_HOME/platform-tools"


# ==============================================================================

# Added 2020-02-09 (initially for pip user installs, but for anything else too).
PATH="${PATH}:${HOME}/.local/bin/"


# ==============================================================================

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/__tabtab.bash ] && . ~/.config/tabtab/__tabtab.bash || true


# ==============================================================================

[[ -s "${HOME}/perl5" ]] && {
    PATH="${HOME}/perl5/bin${PATH:+:${PATH}}"
    export PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
    export PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
    export PERL_MB_OPT="--install_base \"${HOME}/perl5\""
    export PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"
}


# ==============================================================================

# Added 2021-01-18 as per https://wiki.postmarketos.org/wiki/Installing_pmbootstrap
# eval "$(register-python-argcomplete pmbootstrap)"


# ==============================================================================

# Added 2021-02-11
# for yarn global add (installs)
PATH="${PATH}:${HOME}/.yarn/bin/"


# ==============================================================================

# Added 2021-06-26
export PYTHONSTARTUP="${HOME}/.pythonrc.py"


# ==============================================================================

# Added 2021-09026 mainly to get libimobiledevice (and libplist) working
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"


# ==============================================================================

# Added 2022-03-25
# [ -f ~/.fzf.bash ] && source ~/.fzf.bash


# ==============================================================================

# Added 2022-03-25
[ -f ~/.config/nnn/nnn-autocompletion.bash ] && source ~/.config/nnn/nnn-autocompletion.bash

# ==============================================================================

# Added during installation of broot https://dystroy.org/broot/install-br/
# source "${HOME}"/.config/broot/launcher/bash/br


# ==============================================================================

# Added 2022-07-16 for latest pandoc-crossref build.
PATH="${PATH}:${HOME}/.cabal/bin"


# ==============================================================================

# Changed 2023-03-20
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


# ==============================================================================

# Added 2022-09-22 (and 2022-03-09) for nim (via choosenim).
# PATH="${PATH}:${HOME}/.nimble/bin"


# ==============================================================================

# # Added 2022-10-01 for Rust/Cargo/etc.
# [[ -s "${HOME}/.cargo/bin" ]] && PATH="${PATH}:${HOME}/.cargo/bin"
# [[ -s "${HOME}/.cargo/env" ]] && . "${HOME}/.cargo/env"


# ==============================================================================

# Ensure Python's .pyc files and __pycache__ folders don't clutter working dir.
export PYTHONPYCACHEPREFIX="${HOME}/.cache/python"

# Added 2020-11-23 (but see direnv stuff towards end).
# Removed 2024-05-30 because it didn't play well with direnv.
# Dynamic Python cache dirs based on env vars set by virtualenv.
# source() approach courtesy https://stackoverflow.com/a/9497416
# source () { 
#     if builtin source "$@"
#     then
#         if [[ ! -z "${VIRTUAL_ENV}" ]] && [[ ! -z "${_OLD_VIRTUAL_PATH}" ]]
#         then
#             export PYTHONPYCACHEPREFIX="${PYTHONPYCACHEPREFIX}/"$(echo $(cd $VIRTUAL_ENV/.. 1>/dev/null ; pwd ; cd - 1>/dev/null))
#             . <(pip completion --bash) # Add pip completions for bash. Note: don't use "source"! :)
#         fi
#         return 0
#     else
#         return $?
#     fi
# }


# ==============================================================================

# # Env var for https://keys.pub
# [[ -f "${HOME}/.keys.sh" ]] && source "${HOME}/.keys.sh"


# ==============================================================================

# Added 2023-12-18 during re-install of pnpm.
# pnpm
export PNPM_HOME="/home/austinjp/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end



# ==============================================================================

# Added 2023-10-14 to "fix" error messages after uninstalling a thing.
unset LD_PRELOAD


# ==============================================================================

# Added 2023-10-23
PATH="${PATH}:${HOME}/.luarocks/bin/"


# ==============================================================================

# Added 2023-11-27 temporarily: disable left control, shift, and caps-lock keys.
# xmodmap -e 'keycode 37 50 66=' 2>/dev/null

# ==============================================================================

# Added 2024-01-15 for adb and fastboot.
PATH="${PATH}:${HOME}/adb-fastboot"


# ==============================================================================

# # Added 2024-09-25 for https://github.com/sugarme/gotch
# # which is used for Go transformers and tokenizers.
# export GOTCH_LIBTORCH="/usr/lib/x86_64-linux-gnu"
# export LIBRARY_PATH="$LIBRARY_PATH:$GOTCH_LIBTORCH/lib"
# export CPATH="$CPATH:$GOTCH_LIBTORCH/lib:$GOTCH_LIBTORCH/include:$GOTCH_LIBTORCH/include/torch/csrc/api/include"
# export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$GOTCH_LIBTORCH/lib"

# ==============================================================================

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
# [[ -s "${HOME}/.rvm/bin" ]] & PATH="${PATH}:${HOME}/.rvm/bin"
# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# ==============================================================================

# Remove anything from PATH that doesn't exist.

export PATH=$(for d in $(echo "${PATH}" | cut -d':' -f 1,-999 --output-delimiter=$'\n') ; do [[ -s "${d}" ]] && echo $d ; done | tr $'\n' ':' | sed -re 's/\:$//')

# ==============================================================================

# Add any node modules.
# Update: Use direnv for this.
# export PATH="${PATH}:node_modules/.bin"


# ==============================================================================

# Direnv.

eval "$(direnv hook bash)"

# ==============================================================================

function _aliases_add() {
    source ./.aliases 2>/dev/null || :
}

function cd() {
    # Kudos https://askubuntu.com/a/1465614

    # Before leaving dir, unset aliases.
    if [[ -f ./.aliases ]]; then
        # Remove custom aliases
        kill_list=$(sed -re 's/^alias ([^=]+).+/\1/g' ./.aliases | tr $'\n' ' ')
        echo "Reverting aliases: ${kill_list}"
        unalias $kill_list || :

        # Then restore all previous aliases:
        source ~/.bash_aliases 2>/dev/null || :
    fi

    # Do the actual "cd".
    [[ -z "$*" ]] && builtin cd $HOME >/dev/null || :
    [[ -n "$*" ]] && builtin cd "$*"  >/dev/null || :

    # Add local aliases.
    if [[ -f ./.aliases ]]; then
        add_list=$(sed -re 's/^alias ([^=]+).+/\1/g' ./.aliases | tr $'\n' ' ')
        echo "Aliasing: ${add_list}"
        _aliases_add
    fi
}

# Hook to ensure aliases are added whenever prompt is displayed.
_aliases_hook() {
    # This inspired by (ripped off from) direnv.
    local previous_exit_status=$?;
    trap -- '' SIGINT;

    _aliases_add

    trap - SIGINT;
    return $previous_exit_status;
};

if ! [[ "${PROMPT_COMMAND:-}" =~ _aliases_hook ]]; then
    PROMPT_COMMAND="_aliases_hook${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
fi
