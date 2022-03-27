alias ag='ag --css --html --js --json --markdown --python --sass --shell --yaml -t'
alias bat=batcat
# alias chromium='flatpak run com.github.Eloston.UngoogledChromium'
alias chromium='/usr/bin/flatpak run --branch=stable --arch=x86_64 --command=/app/bin/chromium --file-forwarding com.github.Eloston.UngoogledChromium @@u %U @@'
alias docker="podman"
alias dotfile='git --git-dir=$HOME/.dotfiles/.git --work-tree=$HOME/.dotfiles/ "$@"'
alias dotfiles=dotfile
alias emacs="emacsclient -t -c -a '' ${@}"
alias less="less -XRS"
alias ls="ls -h --color=auto --time-style=long-iso"
alias pgrep="pgrep -fila"
alias podman='source <(/usr/local/bin/podman completion bash) && unalias podman && /usr/local/bin/podman "$@"'
alias remmina='flatpak run org.remmina.Remmina'
alias skype="flatpak run com.skype.Client"
alias spotify="flatpak run com.spotify.Client"
alias tree="tree --ignore-case"
