set -x PATH $HOME/bin $PATH
set -x PATH $HOME/.cargo/bin $PATH

set -x NO_AT_BRIDGE 1

set -x TERM 'xterm-256color'

set fish_function_path $fish_function_path "/home/pliniker/.local/lib/python3.6/site-packages/powerline/bindings/fish"
powerline-setup

alias ll="ls -ltrah"
alias open="xdg-open"
alias sbcl="rlwrap sbcl"
alias hy="hy --repl-output-fn=hy.contrib.hy-repr.hy-repr"
alias emacs="env TERM=xterm-24bit /usr/bin/emacs"


# vanilla vim, don't load spacevim
alias vim="vim.nox -u ~/.vimrc-vim.nox"
