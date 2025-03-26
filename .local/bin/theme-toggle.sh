#!/bin/bash

wd=$(realpath "$(cd -- $(dirname -- "${BASH_SOURCE[0]}") &> /dev/null && pwd)")

chrome_is=light

if [ $(/usr/bin/xfconf-query --create -c 'xsettings' -p '/Net/ThemeName' | grep -iE '(dark|night)') ]; then
    chrome_is=dark
fi


if [[ "${chrome_is}" = 'dark' ]] || is_terminal_dark.sh ; then
    "${wd}/theme-light.sh"
else
    "${wd}/theme-dark.sh"
fi
