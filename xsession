#!/bin/bash -x

# quit this script immediately if taffybar is already running to avoid
# rerunning all the below apps, commands and scripts if they already
# were run
if pgrep taffybar > /dev/null ; then
    exit 0
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

# background
nitrogen --restore

# set cursor shape
xsetroot -cursor_name left_ptr

# install taffybar config and launch
mkdir -p ~/.config/taffybar/
ln -sf ~/.xmonad/taffybar.hs ~/.config/taffybar/taffybar.hs
ln -sf ~/.xmonad/taffybar.css ~/.config/taffybar/taffybar.css
taffybar &

# run gnome daemons in the background
gsettings set org.gnome.gnome-flashback desktop false
gsettings set org.gnome.gnome-flashback.desktop show-icons false
gsettings set org.gnome.gnome-flashback.desktop draw-background false
exec gnome-flashback
