#!/bin/bash

if [ -f ~/.config/xfce4/terminal/terminalrc ] ; then
    export bg_hex=$(ag ColorBackground ~/.config/xfce4/terminal/terminalrc | sed -e 's/^.*\=\#//')
    if [ "x${bg_hex}" == "x" ]; then
        # If no background colour is specified, assume it's black.
        export bg_hex=000000
    fi
    export r_hex=$(echo $bg_hex | head -c 2 | tail -c 2)
    export g_hex=$(echo $bg_hex | head -c 4 | tail -c 2)
    export b_hex=$(echo $bg_hex | head -c 6 | tail -c 2)
    if [[ "0x${r_hex}" -lt "0xcc" ]] && [[ "0x${g_hex}" -lt "0xcc" ]] && [[ "0x${b_hex}" -lt "0xcc" ]] ; then
        exit 0 # true
    else
        if [[ "0x${r_hex}" -lt "0xcc" ]] && [[ "0x${g_hex}" -lt "0xcc" ]] ; then
            exit 0 # true
        elif [[ "0x${r_hex}" -lt "0xcc" ]] && [[ "0x${b_hex}" -lt "0xcc" ]] ; then
            exit 0 # true
        elif [[ "0x${g_hex}" -lt "0xcc" ]] && [[ "0x${b_hex}" -lt "0xcc" ]] ; then
            exit 0 #true
        fi
    fi
fi

exit 1 # false
