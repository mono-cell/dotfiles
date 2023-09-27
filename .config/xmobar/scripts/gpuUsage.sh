#!/bin/bash

getGPUusage () {
   local a=$(nvidia-smi | awk 'NR==10{print $13}' | sed 's/%//g')
   tmpPrefix="<fc=green,#262626:0>"
   tmpSuffix="</fc>"
   echo " GPU $a%"
}

getGPUusage &
