#!/bin/bash -x

# run a custom monitor script
if [ -x ~/.xrandr.sh ] ; then
    ~/.xrandr.sh
fi
# run a custom desktop wallpaper script
if [ -x ~/.wallpaper.sh ] ; then
    ~/.wallpaper.sh
fi

ln -sf ~/.xmonad/gtkrc ~/.gtkrc-2.0

if [ -x /usr/bin/xscreensaver ] ; then
    xscreensaver -no-splash &
fi

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

if [ -x /usr/bin/fdpowermon ] ; then
    fdpowermon &
fi

if [ -x /usr/bin/xfce4-volumed ] ; then
    xfce4-volumed
fi

if [ -x ~/.dropbox-dist/dropboxd ] ; then
    ~/.dropbox-dist/dropboxd &
fi

# make capslock a control key
setxkbmap -option ctrl:nocaps

mkdir -p ~/.config/xfce4/terminal/
ln -sf ~/.xmonad/xfce4-terminalrc ~/.config/xfce4/terminal/terminalrc

mkdir -p ~/.config/taffybar/
ln -sf ~/.xmonad/taffybar.hs ~/.config/taffybar/taffybar.hs
exec taffybar
