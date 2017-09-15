#!/bin/bash -ex

sudo apt install -y git openssh-server ibam cpufrequtils xmonad taffybar dmenu notify-osd gnome-terminal gnome-tweak-tool gnote nitrogen feh fdpowermon xfce4-volumed htop emacs vim-nox python-gpgme build-essential curl tzwatch

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
