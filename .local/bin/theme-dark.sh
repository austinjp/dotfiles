#!/bin/bash

wd=$(realpath "$(cd -- $(dirname -- "${BASH_SOURCE[0]}") &> /dev/null && pwd)")

{
    /usr/bin/xfconf-query --create -c 'xsettings' -p '/Net/ThemeName' --type 'string' --set 'Greybird-dark'

    sleep 0.5

    /usr/bin/xfconf-query --create -c 'xfce4-terminal' -p '/color-background' --type 'string' --set '#000000000000'

    /usr/bin/xfconf-query --create -c 'xfce4-terminal' -p '/color-foreground' --type 'string' --set '#ffffffffffff'

	TMPDIR="${HOME}"/.local/tmp emacsclient --socket-name="${HOME}"/.local/tmp/emacs.socket -e "(progn (disable-theme 'austinjp-light) (load-theme 'austinjp-dark t))"
	
    sleep 0.5

    /usr/bin/cp \
        -f \
        /home/austinjp/.config/ghostty/config.night \
        /home/austinjp/.config/ghostty/config

    sleep 0.5

    /usr/bin/cp \
        -f \
        /home/austinjp/.config/ghostty/config.night \
        /home/austinjp/.config/ghostty/config
} &
