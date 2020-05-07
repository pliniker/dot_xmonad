set -x PATH $HOME/bin $PATH
set -x PATH $HOME/local/bin $PATH
set -x PATH $HOME/.cargo/bin $PATH

set -x NO_AT_BRIDGE 1

set -x TERM 'xterm-256color'
# set -x TERM 'xterm-24bit'

set REPO_ROOT ~/.local/lib/python3.7/site-packages/
set fish_function_path $fish_function_path "$REPO_ROOT/powerline/bindings/fish"
powerline-setup

alias ll="ls -ltrah"
alias open="xdg-open"
alias sbcl="rlwrap sbcl"
alias hy="hy --repl-output-fn=hy.contrib.hy-repr.hy-repr"
alias emacs="env TERM=xterm-24bit /usr/bin/emacs"

# vanilla vim, don't load spacevim
alias vim="nvim -u ~/.vimrc-vim.nox"
