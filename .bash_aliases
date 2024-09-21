#!/bin/bash

# alias ag='ag --css --html --js --json --markdown --python --sass --shell --yaml -t'
alias ag='ag --pager="less -XFRi"'


alias _avro2json='java -jar ~/.local/share/json2avro-validator-0.2.3-SNAPSHOT.jar -m avro2json'
function avro2json() { _avro2json "${@}" | ag -v 'schema\.avro\.AvroValidator' ; }

alias bat='batcat --theme $(is_terminal_dark.sh && echo gruvbox-dark || echo OneHalfLight) --decorations never --pager="less -XFRi"'

alias cal="ncal -M -b -h"

# alias cast="mkchromecast"
# alias chromecast="mkchromecast"

# alias chromium='flatpak run com.github.Eloston.UngoogledChromium'
# alias chromium='/usr/bin/flatpak run --branch=stable --arch=x86_64 --command=/app/bin/chromium --file-forwarding com.github.Eloston.UngoogledChromium @@u %U @@'

calc() { printf "%s\n" $(echo "$@" | sed -r -e 's,\s+,,g') | bc -l ; }
alias c=calc

alias cp="cp -i"
alias diff='diff --color=auto --width=$(tput cols)'
alias db="_OLD_VIRTUAL_PATH= VIRTUAL_ENV= devbox"
alias df='df -B 1MB'

# Using functions helps avoid issues with $@ being defined elsewhere.
alias dotfile='_dotfile(){ git --git-dir=$HOME/.dotfiles/.git --work-tree=$HOME/.dotfiles/ "${@}";}; _dotfile'

alias du='du -s -m -P -H'


# ==============================================================================
# Emacs stuff.

EMACS_ALT='nano'
function _emacs() {
    # Change the terminal/tab title:
    printf '\033]2;%s\033\\' 'Emacs'
    mkdir -p "${HOME}"/.local/tmp
    emacs_daemon_is_running=$(\pgrep -fila emacs | ag '( \-\-daemon| \-\-bg\-daemon| \-\-fg\-daemon)');
    if [[ "${emacs_daemon_is_running}" ]]; then
        :
    else
        TMPDIR="${HOME}"/.local/tmp /usr/bin/emacs --daemon="${HOME}"/.local/tmp/emacs.socket ;
    fi
    TMPDIR="${HOME}"/.local/tmp /usr/bin/emacsclient.emacs -t -c -a "${EMACS_ALT}" --socket-name="${HOME}"/.local/tmp/emacs.socket "${@}" ;
}
alias emacs=_emacs
export GIT_EDITOR='TMPDIR='"${HOME}"'/.local/tmp /usr/bin/emacsclient.emacs -t -c -a '"${EMACS_ALT}"' --socket-name='"${HOME}"'/.local/tmp/emacs.socket'

# End of Emacs stuff.
# ==============================================================================


alias fd='fdfind --color=never --follow --glob --no-ignore'

alias free="free -h --giga"
function _git_grep() {
    git rev-list --all | xargs git grep -e "${@}"
}
alias gg=_git_grep
alias gitui='EDITOR=/usr/bin/emacs\ -Q VISUAL=/usr/bin/emacs\ -Q gitui'

function _gauth() {
    if [ "${@}" ]; then
        \gauth | ag -i '^'"${@}" | head -n 1 | sed -r -e 's/\s+/ /g' | cut -d' ' -f 6 | xclip -i -r -selection primary
    else
        \gauth
    fi
}
alias gauth=_gauth

alias hf="huggingface-cli"

alias less="less -XFRi"

# alias libreoffice="/opt/libreoffice6.4/program/soffice"
# alias soffice=libreoffice

alias localsend='flatpak run org.localsend.localsend_app'

function _batman() { /usr/bin/man "${@}" | bat -l man ; }
alias man=_batman

# Make a dir and cd into it.
function mcd () { mkdir -p "${1}" && cd "${1}" ; }

alias mv="mv -i"

alias p="pnpm"

function _pgrep() {
    _pids=$(\pgrep -fila "${@}" | ag -i "${@}" | cut -d' ' -f 1 | tr $'\n' ',' | sed -e 's/,$//g')
    if [[ "${_pids}" ]]; then
        # \ps axw --format 'user,pid,pcpu,%mem,time,command' --pid "${_pids}"
        \ps auxwww q "${_pids}" | \less -XFRSi
    fi
}
alias pgrep=_pgrep


unset docker
unset podman
function _podman(){ type __podman_init_completion >/dev/null 2>/dev/null || source <(\podman completion bash) ; \podman "${@}" ; }
alias podman=_podman
alias docker=podman


# Redshift alias, hard-coded location.
alias redshift='/usr/bin/redshift -l 51.5:0.0 -m randr'  # UK

alias remmina='flatpak run org.remmina.Remmina'
alias skype="flatpak run com.skype.Client"
alias spotify="spt"
alias tree="tree --gitignore --ignore-case -I venv -I 'venv*'"

alias vpn="expressvpn"

alias venv='make_venv -s -v 3.10 && source venv/bin/activate && ln -sr venv/bin/activate -t .'
# alias zotero='env BAMF_DESKTOP_FILE_HINT=/var/lib/snapd/desktop/applications/zotero-snap_zotero-snap.desktop /snap/bin/zotero-snap -c "$(dirname $(readlink -f %k))/zotero -url %U"'
alias zotero='flatpak --filesystem=/home/austinjp/ run org.zotero.Zotero'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls="ls -h --color=auto --time-style=long-iso"
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Prefix output of ls -1 with a running count.
function lsc() {
    ls -1 "${@}" | awk 'BEGIN { a=0; } { b=++a; print b"\t"$1 }'
}

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

function hr() {
    printf %"$(tput cols 2>/dev/null || echo 80)"s | tr " " "_"
}

# Combine which and bat, something I do plenty!
wat() { bat $(which "${1}") ; }


# Carbonyl renders Chrome into the terminal, similar to browsh.
function carbonyl() {
    podman run --rm -ti fathyb/carbonyl "${@}"
}

# Detect if a Python virtual env is *available* (not neccesarily in use).
function find_py_venv() {
    {
        find -maxdepth 3 -type f -name activate -exec ag -l --unrestricted VIRTUAL_ENV {} \; 2>/dev/null || :
    } | sort --version-sort | head -n 1
}

function auto_activate_py_venv() {
    PV=$(find_py_venv)
    if [[ "${PV}" != "" ]] ; then
        source "${PV}"
    fi
    unset PV
}


# ==============================================================================
# Delay some aliases/functions/etc until they are requested, to prevent slow shell startup.

unset nvm
function nvm() {
    [[ "${NVM_BIN}" ]] || {
        export NVM_DIR="$HOME/.nvm"
        [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
        [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
        nvm "${@}"
    }
}

# Added 2022-03-09 via sdkman.io, to get Android and svelte-native working.
unset sdk
function sdk() {
    export SDKMAN_DIR="${HOME}/.sdkman"
    [[ -s "${HOME}/.sdkman/bin/sdkman-init.sh" ]] && source "${HOME}/.sdkman/bin/sdkman-init.sh"
    sdk "${@}"
}

# End of delays.
# ==============================================================================
