#!/bin/bash
watch -n 15 "sensors; awk '{print \$1*10^-6 \" W\"}' /sys/class/power_supply/BAT0/power_now; echo; ibam -a; echo; grep \"cpu MHz\" /proc/cpuinfo"
