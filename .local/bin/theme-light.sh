#!/bin/bash

wd=$(realpath "$(cd -- $(dirname -- "${BASH_SOURCE[0]}") &> /dev/null && pwd)")

{
    /usr/bin/xfconf-query --create -c 'xsettings' -p '/Net/ThemeName' --type 'string' --set 'Greybird'

    sleep 0.5

    /usr/bin/xfconf-query --create -c 'xfce4-terminal' -p '/color-background' --type 'string' --set '#ffffffffffff'

    /usr/bin/xfconf-query --create -c 'xfce4-terminal' -p '/color-foreground' --type 'string' --set '#000000000000'

    sleep 0.5

    /usr/bin/cp \
        -f \
        /home/austinjp/.config/ghostty/config.day \
        /home/austinjp/.config/ghostty/config

    sleep 0.5

    /usr/bin/cp \
        -f \
        /home/austinjp/.config/ghostty/config.day \
        /home/austinjp/.config/ghostty/config

    sleep 1
} &
