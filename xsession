#!/bin/bash

# if it looks like we're running in a virtualbox vm, try setting 3 monitors
if [[ `lspci | grep VirtualBox` != "" ]]; then
    xrandr --output VBOX0 --mode 1920x1080 --primary --auto --output VBOX1 --mode 1920x1080 --right-of VBOX0 --auto --output VBOX2 --mode 1920x1080 --auto --right-of VBOX1
fi

xscreensaver -no-splash &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

mkdir -p ~/.config/taffybar/
ln -sf ~/.xmonad/taffybar.hs ~/.config/taffybar/taffybar.hs
exec taffybar
