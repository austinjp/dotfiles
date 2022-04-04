alias ag='ag --css --html --js --json --markdown --python --sass --shell --yaml -t'
alias bat=batcat
# alias chromium='flatpak run com.github.Eloston.UngoogledChromium'
alias chromium='/usr/bin/flatpak run --branch=stable --arch=x86_64 --command=/app/bin/chromium --file-forwarding com.github.Eloston.UngoogledChromium @@u %U @@'
alias docker="podman"
alias dotfile='git --git-dir=$HOME/.dotfiles/.git --work-tree=$HOME/.dotfiles/ "$@"'
alias dotfiles=dotfile
alias emacs="emacsclient -t -c -a '' ${@}"
alias less="less -XFRS"
alias pgrep="pgrep -fila"
alias podman='source <(/usr/local/bin/podman completion bash) && unalias podman && /usr/local/bin/podman "$@"'
alias remmina='flatpak run org.remmina.Remmina'
alias skype="flatpak run com.skype.Client"
alias spotify="flatpak run com.spotify.Client"
alias tree="tree --ignore-case"


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

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
