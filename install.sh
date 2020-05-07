#!/bin/bash -ex

sudo apt install -y \
    git openssh-server fish htop curl build-essential emacs vim-nox \
    xmonad taffybar dmenu gnome-flashback gnome-session-flashback gnome-terminal gnome-tweak-tool gnote nitrogen formiko \
    ibam powertop tlp

#
# Setup gnome flashback session manager, prevent nautilus from auto-starting
#
sudo cp gnome-flashback-xmonad.session /usr/share/gnome-session/sessions/
sudo rm /etc/xdg/autostart/*nautilus*

#
# Install gnome-terminal profiles
#
# (To dump gnome-terminal profiles:)
# dconf dump /org/gnome/terminal/legacy/profiles:/ > gnome-term.conf
#
cat gnome-term.conf | dconf load /org/gnome/terminal/legacy/profiles:/

#
# Install powerline, adjusting REPO_ROOT as appropriate
#
pip3 install --user powerline-status

#REPO_ROOT=~/.local/lib/python3.6/site-packages/ 
#echo set fish_function_path \$fish_function_path "$REPO_ROOT/powerline/bindings/fish" >> ~/.config/fish/config.fish
#echo powerline-setup >> ~/.config/fish/config.fish

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
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
git clone git@github.com/pliniker/dot_spacemacs ~/.spacemacs.d

#
# Install spacevim
#
curl -sLf https://spacevim.org/install.sh | bash
git clone git@github.com/pliniker/dot_spacevim_d ~/.SpaceVim.d

#
# Install python deps
#
pip3 install --user 'python-language-server[all]'

#
# Install neovim
#
curl -L https://github.com/neovim/neovim/releases/download/stable/nvim.appimage -o ~/bin/nvim
chmod +x ~/bin/nvim

#
# Install vanilla vimrc
#
cp vimrc ~/.vimrc-vim.nox

# Enable explicit true color terminal
# https://www.gnu.org/software/emacs/manual/html_mono/efaq.html#Colors-on-a-TTY
#
tic -x -o ~/.terminfo terminfo-24bit.src
