#!/bin/bash -ex

#
# Install environment
#
sudo apt install -y git openssh-server fish htop curl build-essential vim-nox

#
# Install alacritty
#
# https://github.com/alacritty/alacritty/releases/latest
cp -r alacritty ~/.config/

#
# Fish shell config
#
mkdir -i ~/.config/fish/
cp fish/config.fish ~/.config/fish/

#
# Install spacevim
#
curl -sLf https://spacevim.org/install.sh | bash
git clone git@github.com/pliniker/dot_spacevim_d ~/.SpaceVim.d

#
# Install python deps
#
pip3 install --user powerline-status
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

