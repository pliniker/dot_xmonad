set -x PATH $HOME/bin $PATH
set -x PATH $HOME/.local/bin $PATH

set -x NO_AT_BRIDGE 1
set -x DISPLAY :0.0
set -x LIBGL_ALWAYS_INDIRECT 1
set -x TERM 'xterm-256color'

set -x DOCKER_HOST tcp://localhost:2375

#set PYTHON_VER python3.6
#set PYTHON_ROOT ~/.local/lib/$PYTHON_VER/site-packages/
#set fish_function_path $fish_function_path "$PYTHON_ROOT/powerline/bindings/fish"
#powerline-setup

alias ll="ls -ltrah"
alias open="xdg-open"
alias sbcl="rlwrap sbcl"
alias hy="hy --repl-output-fn=hy.contrib.hy-repr.hy-repr"
alias emacs="env TERM=xterm-24bits /usr/bin/emacs"
alias vim="vim -u ~/.vimrc-vim.nox"
alias python=python3.8
alias python3=python3.8

source $HOME/.keychain/LV59ZN5Y2-fish

umask 0022
