#!/bin/bash -x

# run a custom monitor script
if [ -x ~/.xrandr.sh ] ; then
    ~/.xrandr.sh
fi

if [ -x /usr/bin/compton ] ; then
    # Don't run this by default, it degrades X responsiveness
    #compton -e 1.0 -i 0.95 -f -D3 -b
    echo nocompton
fi

# run a custom desktop wallpaper script
if [ -x ~/.wallpaper.sh ] ; then
    ~/.wallpaper.sh
fi

if [ -x /usr/lib/x86_64-linux-gnu/notify-osd ] ; then
    /usr/lib/x86_64-linux-gnu/notify-osd &
fi

if [ -x /usr/bin/gnome-screensaver ] ; then
    gnome-screensaver &
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

if [ -x /usr/bin/keychain ] ; then
    keychain
fi

if [ -x ~/.dropbox-dist/dropboxd ] ; then
    ~/.dropbox-dist/dropboxd &
fi

if [ -x ~/.local/bin/powerline-daemon ] ; then
    powerline-daemon
fi

if [ -x /usr/bin/redshift-gtk ] ; then
    redshift-gtk &
fi

# make capslock a control key
setxkbmap -option ctrl:nocaps

# install fonts
if [ ! -d ~/.fonts ] ; then
    mkdir -p ~/.fonts
    cp -r ~/.xmonad/fonts/*.ttf ~/.fonts/
    fc-cache -fv ~/.fonts/
fi

# install taffybar config and launch
mkdir -p ~/.config/taffybar/
ln -sf ~/.xmonad/taffybar.hs ~/.config/taffybar/taffybar.hs
exec taffybar
