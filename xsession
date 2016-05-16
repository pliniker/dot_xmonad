#!/bin/bash -x

ln -sf ~/.xmonad/gtkrc ~/.gtkrc-2.0

# run a custom monitor script
if [ -x ~/.xrandr.sh ] ; then
    ~/.xrandr.sh
fi
# run a custom desktop wallpaper script
if [ -x ~/.wallpaper.sh ] ; then
    ~/.wallpaper.sh
fi

if [ -x /usr/bin/gnome-screensaver ] ; then
    gnome-screensaver -no-splash &
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
mkdir -p ~/.fonts
cp -r ~/.xmonad/fonts/*.ttf ~/.fonts/
fc-cache -fv

# install xfce4-terminal config
mkdir -p ~/.config/xfce4/terminal/
ln -sf ~/.xmonad/xfce4-terminalrc ~/.config/xfce4/terminal/terminalrc

# install taffybar config and launch
mkdir -p ~/.config/taffybar/
ln -sf ~/.xmonad/taffybar.hs ~/.config/taffybar/taffybar.hs
exec taffybar
