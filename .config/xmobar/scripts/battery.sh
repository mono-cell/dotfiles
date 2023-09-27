#!/bin/bash
cap="$(cat /sys/class/power_supply/BAT0/capacity)"
state=" "
echo "<fc=black,#67EC67:0>  $state </fc><fc=#67EC67,#262626:0> $cap </fc>"
