#!/bin/bash

# This script is for fetching the CPU and GPU temperature and
# echo the output for use with a status bar such as XMobar


# draft cpu temp command:
# sensors | grep "CPU Temp" | awk '{ print $3 }' | sed 's/[+ -]//g'

# draft gpu temp command:
# nvidia-smi | awk 'NR==10{print $3}'

getCPUtemp () {
    local cmd=$(sensors | grep "CPU Temp" | awk '{ print $3 }' | sed 's/[+ - C]//g'\
              | iconv -c -f utf-8 -t ascii 2>&1)

    if [[ ${cmd} -ge 50 ]]; then
        local tmpPrefix="<fc=darkOrange,#262626:0>"
        local tmpSuffix="</fc>"
    fi

    if [[ ${cmd} -ge 80 ]]; then
        local tmpPrefix="<fc=red,#262626:0>"
        local tmpSuffix="</fc>"
    fi

    printf "<fn=1>﬙ </fn>CPU "; printf "$tmpPrefix"; printf %.0f "${cmd}"; printf "$tmpSuffix"; printf "°C";
    # remove & round floats from output aswell
}

getGPUtemp () {
    local cmd=$(nvidia-smi | awk 'NR==10{print $3}' | sed 's/C//g' 2>&1)

    if [[ ${cmd} -ge 50 ]]; then
        local tmpPrefix="<fc=darkOrange,#262626:0>"
        local tmpSuffix="</fc>"
    fi

    if [[ ${cmd} -ge 80 ]]; then
        local tmpPrefix="<fc=red,#262626:0>"
        local tmpSuffix="</fc>"
    fi

    printf " GPU $tmpPrefix${cmd}$tmpSuffix°C"
}

printTemp () {
    local sep=" "
    printf ""
    getCPUtemp; printf "$sep"; getGPUtemp
}

printTemp
