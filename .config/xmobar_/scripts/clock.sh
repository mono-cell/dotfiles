#! /bin/sh

date="$(date +"%a %b %d %l:%M %p"| sed 's/  / /g')"
echo "<fc=black,#67EC67:0>   </fc><fc=#67EC67,#262626:0> $date </fc>"
