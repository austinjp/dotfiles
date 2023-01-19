# alias ag='ag --css --html --js --json --markdown --python --sass --shell --yaml -t'
alias bat='batcat --theme $(is_terminal_dark.sh && echo gruvbox-dark || echo OneHalfLight) --decorations never --pager="less -XFRSi"'
# alias cast="mkchromecast"
# alias chromecast="mkchromecast"

# alias chromium='flatpak run com.github.Eloston.UngoogledChromium'
# alias chromium='/usr/bin/flatpak run --branch=stable --arch=x86_64 --command=/app/bin/chromium --file-forwarding com.github.Eloston.UngoogledChromium @@u %U @@'

alias cp="cp -i"
alias diff='diff --color=auto --width=$(tput cols)'
alias db="_OLD_VIRTUAL_PATH= VIRTUAL_ENV= devbox"
alias df='df -B 1MB'
alias docker="podman"

# Using functions helps avoid issues with $@ being defined elsewhere.
alias dotfile='_dotfile(){ git --git-dir=$HOME/.dotfiles/.git --work-tree=$HOME/.dotfiles/ "${@}";}; _dotfile'

alias du='du -s -m -P -H'

# The -a arg uses $ALTERNATIVE_EDITOR but when set to blank string,
# it starts an emacs daemon :)
alias emacs="emacsclient -t -c -a \"\""

alias free="free -h --giga"
alias less="less -XFRS"
alias mv="mv -i"
alias p="pnpm"
alias pgrep="pgrep -fila"
alias podman='_podman(){ type __podman_init_completion >/dev/null 2>/dev/null || source <(\podman completion bash) ; \podman "${@}" ; }; _podman'
alias redshift='redshift -l 51.5:0.0 -m randr'
alias remmina='flatpak run org.remmina.Remmina'
alias skype="flatpak run com.skype.Client"
alias spotify="spt"
alias tree="tree --ignore-case"
alias venv='python -m venv venv-$(python --version | cut -f 2 -d" " | cut -f 1-2 -d ".") && ln -s venv-$(python --version | cut -f 2 -d" " | cut -f 1-2 -d ".") venv'
alias zotero='env BAMF_DESKTOP_FILE_HINT=/var/lib/snapd/desktop/applications/zotero-snap_zotero-snap.desktop /snap/bin/zotero-snap -c "$(dirname $(readlink -f %k))/zotero -url %U"'

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
