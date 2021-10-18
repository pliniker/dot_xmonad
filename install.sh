#!/bin/bash -ex

#
# Install environment
#
sudo apt install -y \
    git openssh-server fish htop curl build-essential emacs-nox vim-nox \
    xmonad taffybar dmenu gnome-flashback gnome-session-flashback gnome-terminal gnome-tweak-tool gnote nitrogen \
    ibam powertop tlp

#
# Setup gnome flashback session manager, prevent nautilus from auto-starting
#
sudo cp gnome/gnome-flashback-xmonad.session /usr/share/gnome-session/sessions/
sudo rm /etc/xdg/autostart/*nautilus*

#
# Install gnome-terminal profiles
#
# (To dump gnome-terminal profiles:)
# dconf dump /org/gnome/terminal/legacy/profiles:/ > gnome-term.conf
#
cat gnome-term.conf | dconf load /org/gnome/terminal/legacy/profiles:/

#
# Install alacritty
#
# https://github.com/alacritty/alacritty/releases/latest
cp -r alacritty ~/.config/

#
# Handy scripts
#
mkdir ~/bin/
cp tools/* ~/bin/

#
# Fish shell config
#
mkdir -i ~/.config/fish/
cp fish/config.fish ~/.config/fish/

#
# Install spacemacs
#
git clone https://github.com/syl20bnr/spacemacs -b develop ~/.emacs.d
git clone git@github.com:pliniker/dot_spacemacs_d ~/.spacemacs.d

#
# Install neovim
#
curl -L https://github.com/neovim/neovim/releases/download/stable/nvim.appimage -o ~/bin/nvim
chmod +x ~/bin/nvim

#
# Install vimrc
#
mkdir -p ~/.config/nvim/
cp nvim/init.lua ~/.config/nvim/init.lua
cp vimrc ~/.vimrc-vim.nox

# Enable explicit true color terminal
# https://www.gnu.org/software/emacs/manual/html_mono/efaq.html#Colors-on-a-TTY
#
tic -x -o ~/.terminfo terminfo-24bit.src

#
# Install tools
#
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install exa
cargo install battop
cargo install ripgrep   # or download
cargo install zellij    # or download it
cargo install starship  # or download
