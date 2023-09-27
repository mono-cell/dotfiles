#!/bin/bash

dockerStats=$(docker ps -q | wc -l)

box1="<fc=#000000,#67EC67:0>"
box1_suffix="</fc>"

box2="<fc=#67EC67,#262626:0>"
box2_suffix="</fc>"

offline_msg="$box1  Docker: $box1_suffix$box2 $dockerStats (Down) $box2_suffix"
online_msg="$box1  Docker: $box1_suffix$box2 $dockerStats (Up) $box2_suffix"

if [ "$dockerStats" -lt 1 ]; then       # for offline containers
    echo "$offline_msg"
fi

if [ "$dockerStats" -gt 0 ]; then       # for online containers
    echo "$online_msg"
fi
