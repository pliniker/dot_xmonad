#!/bin/bash
watch -n 15 "awk '{print \"Battery discharge rate:     \" \$1*10^-6*7.6 \" W\"}' /sys/class/power_supply/BAT0/current_now; ibam --batterybios --percentbios; echo; grep \"cpu MHz\" /proc/cpuinfo; echo; sensors dell_smm-virtual-0"

