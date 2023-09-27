#!/bin/bash

charge=$(cat "/sys/class/power_supply/sony_controller_battery_dc:0c:2d:75:f9:b5/capacity")
disconnected="<fc=black,#67EC67:0>  DS4: </fc><fc=#67EC67,#262626:0> n/c </fc> "
battery="<fc=black,#67EC67:0>  DS4: </fc><fc=#67EC67,#262626:0> ${charge}% </fc> "

if [[ "${charge}" -eq " " ]]; then
    echo "$disconnected"
fi

echo "$battery"
