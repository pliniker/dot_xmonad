#!/bin/bash -x

# run a custom monitor script
if [ -x ~/.xrandr.sh ] ; then
    ~/.xrandr.sh
fi

if [ -x /usr/bin/compton ] ; then
    compton -e 1.0 -i 0.9 -f -D3 -b
    #xcompmgr -cfF -t-9 -l-11 -r9 -o.95 -D3 &
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

# make capslock a control key
setxkbmap -option ctrl:nocaps

# install fonts
if [ ! -d ~/.fonts ] ; then
    mkdir -p ~/.fonts
    cp -r ~/.xmonad/fonts/*.ttf ~/.fonts/
    fc-cache -fv
fi

# install taffybar config and launch
mkdir -p ~/.config/taffybar/
ln -sf ~/.xmonad/taffybar.hs ~/.config/taffybar/taffybar.hs
exec taffybar
