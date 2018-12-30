#!/bin/bash -ex

sudo apt install -y git openssh-server fish xmonad taffybar dmenu \
    gnome-terminal gnome-tweak-tool gnote nitrogen htop emacs vim-nox \
    build-essential curl ibam powertop tlp cbatticon

#
# setup gnome session manager
#
sudo apt install -y gnome-session-flashback
sudo cp gnome-flashback-xmonad /usr/share/gnome-session/sessions/
sudo rm /etc/xdg/autostart/nautilus*

#
# Turn off dnsmasq:
#
# sudo vim /etc/NetworkManager/NetworkManager.conf
# comment-out dns=dnsmasq
# sudo service network-manager restart
#

#
# Install spacemacs
#
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

#
# To dump and restore gnome-terminal profiles:
#
# dconf dump /org/gnome/terminal/legacy/profiles:/ > gnome-term.conf
#
cat gnome-term.conf | dconf load /org/gnome/terminal/legacy/profiles:/

mkdir -p ~/.config/gtk-3.0/
cp gtk3-settings.ini ~/.config/gtk-3.0/settings.ini

#
# Install powerline, adjusting REPO_ROOT as appropriate
#
pip3 install --user powerline-status

REPO_ROOT=~/.local/lib/python3.5/site-packages/ 
echo set fish_function_path \$fish_function_path "$REPO_ROOT/powerline/bindings/fish" >> ~/.config/fish/config.fish
echo powerline-setup >> ~/.config/fish/config.fish

#
# Configure redshift
#
cp redshift.conf ~/.config/redshift.conf
