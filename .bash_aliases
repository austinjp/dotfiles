my_emacs() {
    if [[ x`pgrep emacs 2>/dev/null` = x ]] ; then
	\emacs --daemon
    fi
    emacsclient -c "$@"
}

alias emacs=my_emacs
alias less="less -XRS"
alias ls="ls -h --color=auto --time-style=long-iso"
alias pgrep="pgrep -fila"
alias docker="podman"
alias tree="tree --ignore-case"
alias spotify="flatpak run com.spotify.Client"
alias skype="flatpak run com.skype.Client"
alias remmina='flatpak run org.remmina.Remmina'

alias ag='ag --css --html --js --json --markdown --python --sass --shell --yaml -t'

# alias chromium='flatpak run com.github.Eloston.UngoogledChromium'

alias dotfile='git --git-dir=$HOME/.dotfiles/.git --work-tree=$HOME/.dotfiles/ "$@"'
alias dotfiles=dotfile

alias podman='source <(/usr/local/bin/podman completion bash) && unalias podman && /usr/local/bin/podman "$@"'
