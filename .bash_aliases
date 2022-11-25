alias ag='ag --css --html --js --json --markdown --python --sass --shell --yaml -t'
alias bat='batcat --pager="less -XFRS"'
alias cast="mkchromecast"
alias chromecast="mkchromecast"
# alias chromium='flatpak run com.github.Eloston.UngoogledChromium'
alias chromium='/usr/bin/flatpak run --branch=stable --arch=x86_64 --command=/app/bin/chromium --file-forwarding com.github.Eloston.UngoogledChromium @@u %U @@'
alias cp="cp -i"
alias db="_OLD_VIRTUAL_PATH= VIRTUAL_ENV= devbox"
alias df='df -B 1MB'
alias docker="podman"

# Using functions helps avoid issues with $@ being defined elsewhere.
alias dotfile='_dotfile(){ git --git-dir=$HOME/.dotfiles/.git --work-tree=$HOME/.dotfiles/ "${@}";}; _dotfile'

alias du='du --dereference-args --total -B 1MB'
alias emacs='_emx(){ emacsclient -t -c -a "" "${@}";}; _emx'
alias free="free -h --giga"
alias less="less -XFRS"
alias mv="mv -i"
alias p="pnpm"
alias pgrep="pgrep -fila"
alias podman='_podman(){ source <(/usr/bin/podman completion bash) && unalias podman && /usr/bin/podman "${@}";}; _podman ; unset _podman'
alias remmina='flatpak run org.remmina.Remmina'
alias skype="flatpak run com.skype.Client"
alias spotify="spt"
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

