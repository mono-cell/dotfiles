#!/usr/bin/env bash
# This script uses checkupdates + AUR (from the AUR) to also check for updates to
# AUR packages while checking for non-AUR package updates aswell.

cupd=$(checkupdates | wc -l)
echo "<fc=black,#67EC67:0>  UPDATES: </fc><fc=#67EC67,#262626:0> $cupd UPDATES </fc>"
