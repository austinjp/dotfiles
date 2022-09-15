# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[[01;32m\]\u@\h\[[00m\]:\[[01;34m\]\w\[[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\]$PS1"
    ;;
*)
    ;;
esac

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Alias definitions.
unalias -a
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

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

# Added 2021-08-11
# source ~/.bashrc.oh-my-bash

export PAGER='less'

# Added 2019-09-12: use specific ssh key in Git:
# export GIT_SSH_COMMAND="ssh -i ~/.ssh/id_bitbucket"
# Updated 2020-04-10
# Using ~/.ssh/config file instead.
# The following line ensures ssh-add can be used to
# add keys to gnome-keyring, the GUI key auth thing.
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/keyring/ssh"

export PATH="${PATH}:~/bin/"

# Added 2020-01-08 for Android Studio
# as per https://facebook.github.io/react-native/docs/getting-started
export ANDROID_HOME="${HOME}/Android/Sdk"
export PATH="${PATH}:$ANDROID_HOME/emulator"
export PATH="${PATH}:$ANDROID_HOME/tools"
export PATH="${PATH}:$ANDROID_HOME/tools/bin"
export PATH="${PATH}:$ANDROID_HOME/platform-tools"

# Added 2020-02-09 for pip user installs.
export PATH="${PATH}:~/.local/bin/"

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/__tabtab.bash ] && . ~/.config/tabtab/__tabtab.bash || true

export $(gnome-keyring-daemon --daemonize --start)

export PATH="${HOME}/perl5/bin${PATH:+:${PATH}}"
export PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_MB_OPT="--install_base \"${HOME}/perl5\""
export PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"

# Added 2020-11-23
# Dynamic Python cache dirs based on env vars set by virtualenv.
# source() approach courtesy https://stackoverflow.com/a/9497416
unset PYTHONPYCACHEPREFIX
source () { 
    if builtin source "$@"
    then
        if [[ ! -z "${VIRTUAL_ENV}" ]] && [[ ! -z "${_OLD_VIRTUAL_PATH}" ]]
        then
            export PYTHONPYCACHEPREFIX="/tmp/pycache/"$(echo $(cd $VIRTUAL_ENV/.. 1>/dev/null ; pwd ; cd - 1>/dev/null))
            . <(pip completion --bash) # Add pip completions for bash. Note: don't use "source"! :)
        else
            unset PYTHONPYCACHEPREFIX
        fi
        return 0
    else
        return $?
    fi
}

# Added 2020-11-25
export PATH="${PATH}:${HOME}/go/bin:node_modules/.bin"

# Added 2021-01-18 as per https://wiki.postmarketos.org/wiki/Installing_pmbootstrap
eval "$(register-python-argcomplete pmbootstrap)"

# Added 2021-02-11
# for yarn global add (installs)
export PATH="${PATH}:${HOME}/.yarn/bin/"

# Added 2021-06-06
# mainly for 'git commit'.
export EDITOR='emacsclient -c "$@"'

# Added 2021-06-26
export PYTHONSTARTUP="${HOME}/.pythonrc.py"

# Added 2021-09026 mainly to get libimobiledevice (and libplist) working
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"

# Added 2022-03-09 for nim (via choosenim).
export PATH="${PATH}:/home/austinjp/.nimble/bin"

# Added 2022-03-25
# [ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Added 2022-03-25
[ -f ~/.config/nnn/nnn-autocompletion.bash ] && source ~/.config/nnn/nnn-autocompletion.bash

# Added during installation of broot https://dystroy.org/broot/install-br/
# source /home/austinjp/.config/broot/launcher/bash/br

# Added 2022-04-18 for delta git pager.
export DELTA_PAGER="less -XFRS"

# Added 2022-06-15 for functiontrace server.
export PATH="${PATH}:/home/austinjp/.cargo/bin"

# Added 2022-07-16 for latest pandoc-crossref build.
export PATH="${PATH}:/home/austinjp/.cabal/bin"

# Added 2022-09-12 for go
export GOPATH=/home/austinjp/.local/lib/go/
export GOBIN=/home/austinjp/.local/lib/go/bin/
export PATH="${PATH}:${GOBIN}"

# Added 2022-09-12 for powerline-go
# See https://github.com/justjanne/powerline-go
function _update_ps1() {
    # PS1="$($GOPATH/bin/powerline-go -error $? -jobs $(jobs -p | wc -l))"
    PS1="$($GOPATH/bin/powerline-go -cwd-max-depth 4 -cwd-mode semifancy -error $? -hostname-only-if-ssh -truncate-segment-width 20 -newline)"

    # Uncomment the following line to automatically clear errors after showing
    # them once. This not only clears the error for powerline-go, but also for
    # everything else you run in that shell. Don't enable this if you're not
    # sure this is what you want.
    # set "?"
}
if [ "$TERM" != "linux" ] && [ -f "$GOPATH/bin/powerline-go" ]; then
    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
fi
# Workaround for nix-shell --pure
if [ "$IN_NIX_SHELL" == "pure" ]; then
    if [ -x "$HOME/.nix-profile/bin/powerline-go" ]; then
        alias powerline-go="$HOME/.nix-profile/bin/powerline-go"
    elif [ -x "/run/current-system/sw/bin/powerline-go" ]; then
        alias powerline-go="/run/current-system/sw/bin/powerline-go"
    fi
fi

# Added 2022-03-09 via sdkman.io, to get Android and svelte-native working.
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="${HOME}/.sdkman"
[[ -s "${HOME}/.sdkman/bin/sdkman-init.sh" ]] && source "${HOME}/.sdkman/bin/sdkman-init.sh"

# Do not add anything below here, see sdkman above.
