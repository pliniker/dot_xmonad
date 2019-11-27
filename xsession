#!/bin/bash -x

# run gnome daemons in the background
if [ -x /usr/bin/gnome-flashback ] ; then
    gnome-flashback &
fi

# run a custom monitor script
if [ -x ~/.xrandr.sh ] ; then
    ~/.xrandr.sh
fi

# run a custom desktop wallpaper script
if [ -x ~/.wallpaper.sh ] ; then
    ~/.wallpaper.sh
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

# install fonts
if [ ! -d ~/.fonts ] ; then
    mkdir -p ~/.fonts
    cp -r ~/.xmonad/fonts/*.ttf ~/.fonts/
    fc-cache -fv ~/.fonts/
fi

# make capslock a control key
setxkbmap -option ctrl:nocaps
xmodmap -e "keysym Print = Menu"

# transparency
compton -CGb -i 0.9

# set cursor shape
xsetroot -cursor_name left_ptr

# install taffybar config and launch
mkdir -p ~/.config/taffybar/
ln -sf ~/.xmonad/taffybar.hs ~/.config/taffybar/taffybar.hs
exec taffybar
