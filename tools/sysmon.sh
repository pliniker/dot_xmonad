#!/bin/bash
watch -n 15 "echo; awk '{print \"Battery discharge rate:     \" \$1*10^-6 \" W\"}' /sys/class/power_supply/BAT0/power_now; ibam --batterybios --percentbios; echo; grep \"cpu MHz\" /proc/cpuinfo; echo; sensors"
