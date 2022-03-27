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

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

# Added 2021-08-11
source ~/.bashrc.oh-my-bash
unalias -a
export PAGER='less -XF'


if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Added 2019-09-12: use specific ssh key in Git:
# export GIT_SSH_COMMAND="ssh -i ~/.ssh/id_bitbucket"
# Updated 2020-04-10
# Using ~/.ssh/config file instead.
# The following line ensures ssh-add can be used to
# add keys to gnome-keyring, the GUI key auth thing.
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/keyring/ssh"

export PATH="$PATH:~/bin/"

# Added 2020-01-08 for Android Studio
# as per https://facebook.github.io/react-native/docs/getting-started
export ANDROID_HOME=$HOME/Android/Sdk
export PATH="$PATH:$ANDROID_HOME/emulator"
export PATH="$PATH:$ANDROID_HOME/tools"
export PATH="$PATH:$ANDROID_HOME/tools/bin"
export PATH="$PATH:$ANDROID_HOME/platform-tools"

# Added 2020-02-09 for pip user installs.
export PATH="$PATH:~/.local/bin/"

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/__tabtab.bash ] && . ~/.config/tabtab/__tabtab.bash || true

export $(gnome-keyring-daemon --daemonize --start)

PATH="${HOME}/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"${HOME}/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"; export PERL_MM_OPT;

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
        else
            unset PYTHONPYCACHEPREFIX
        fi
        return 0
    else
        return $?
    fi
}

# Added 2020-11-25
PATH="${HOME}/go/bin:${PATH}" ; export PATH
PATH="node_modules/.bin:${PATH}" ; export PATH

# Added 2021-01-18 as per https://wiki.postmarketos.org/wiki/Installing_pmbootstrap
eval "$(register-python-argcomplete pmbootstrap)"

# Added 2021-02-11
# for yarn global add (installs)
PATH="${HOME}/.yarn/bin/:${PATH}"; export PATH

# Added 2021-06-06
# mainly for 'git commit'.
EDITOR='emacsclient -c "$@"'; export EDITOR

# Added 2021-06-26
export PYTHONSTARTUP="${HOME}/.pythonrc.py"

# Added 2021-09026 mainly to get libimobiledevice (and libplist) working
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"

# Added 2022-03-09 for nim (via choosenim).
export PATH=/home/austinjp/.nimble/bin:$PATH

# Added 2022-03-09 via sdkman.io, to get Android and svelte-native working.
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# Added 2022-03-25
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Added 2022-03-25
[ -f ~/.config/nnn/nnn-autocompletion.bash ] && source ~/.config/nnn/nnn-autocompletion.bash
