#!/bin/bash -ex

#
# Setup Debian repos
#
sudo cp -r debian/apt/ /etc/

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
sudo cp gnome/gnome-flashback-xmonad-custom.desktop /usr/share/xsessions/
sudo cp gnome/gnome-flashback-xmonad-custom.session /usr/share/gnome-session/sessions/
sudo cp gnome/gnome-flashback-xmonad-custom /usr/lib/gnome-flashback/
sudo cp gnome/xmonad-custom.desktop /usr/share/applications/

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
git clone git@github.com/pliniker/dot_spacemacs ~/.spacemacs.d

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

#
# Install rust
#
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install battop
