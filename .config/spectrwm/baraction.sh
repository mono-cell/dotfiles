#!/bin/bash
# baraction.sh for spectrwm status bar

## DISK
hdd() {
  hdd="$(df -h | awk 'NR==4{print $3, $5}')"
  echo -e "$hdd"
}

## RAM
mem() {
  mem=`free | awk '/Mem/ {printf "%dM/%dM\n", $3 / 1024.0, $2 / 1024.0 }'`
  echo -e "$mem"
}

## Updates
updates () {
  echo -e "0"
  # updates=$(checkupdates | wc -l)
  # echo -e "$updates"
}

SLEEP_SEC=3
#loops forever outputting a line every SLEEP_SEC secs

# It seems that we are limited to how many characters can be displayed via
# the baraction script output. And the the markup tags count in that limit.
# So I would love to add more functions to this script but it makes the
# echo output too long to display correctly.
while :; do
  echo "+@fg=1; +@fn=0; $(updates) +@fg=0; +@fg=2;  +@fn=0; $(mem) +@fg=0; +@fg=3;  +@fn=0; $(hdd) +@fg=0; +@fg=4;"
	sleep $SLEEP_SEC
done
