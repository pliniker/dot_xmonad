#!/bin/bash -ex

#
# Install environment
#
sudo apt install -y git openssh-server fish htop curl build-essential vim-nox gnome-tweaks powertop tlp keychain

#
# Install gnome-terminal profiles
#
# (To dump gnome-terminal profiles:)
# dconf dump /org/gnome/terminal/legacy/profiles:/ > gnome-term.conf
#
cat gnome-term.conf | dconf load /org/gnome/terminal/legacy/profiles:/

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
# see nvim/init.lua for further steps

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

#
# Setup one-stroke workspace switching
#
gsettings set org.gnome.mutter dynamic-workspaces false
gsettings set org.gnome.desktop.wm.preferences num-workspaces 9
gsettings set org.gnome.shell.keybindings switch-to-application-1  []
gsettings set org.gnome.shell.keybindings switch-to-application-2  []
gsettings set org.gnome.shell.keybindings switch-to-application-3  []
gsettings set org.gnome.shell.keybindings switch-to-application-4  []
gsettings set org.gnome.shell.keybindings switch-to-application-5  []
gsettings set org.gnome.shell.keybindings switch-to-application-6  []
gsettings set org.gnome.shell.keybindings switch-to-application-7  []
gsettings set org.gnome.shell.keybindings switch-to-application-8  []
gsettings set org.gnome.shell.keybindings switch-to-application-9  []
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1  "['<Super>1']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2  "['<Super>2']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3  "['<Super>3']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4  "['<Super>4']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-5  "['<Super>5']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-6  "['<Super>6']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-7  "['<Super>7']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-8  "['<Super>8']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-9  "['<Super>9']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-1  "['<Super><Shift>1']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-2  "['<Super><Shift>2']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-3  "['<Super><Shift>3']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-4  "['<Super><Shift>4']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-5  "['<Super><Shift>5']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-6  "['<Super><Shift>6']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-7  "['<Super><Shift>7']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-8  "['<Super><Shift>8']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-9  "['<Super><Shift>9']"

#
# Install fonts
#
mkdir -p ~/.fonts
cp -r ~/.xmonad/fonts/*.ttf ~/.fonts/
fc-cache -fv ~/.fonts/

