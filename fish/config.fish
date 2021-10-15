set -x PATH $HOME/bin $PATH
set -x PATH $HOME/local/bin $PATH
set -x PATH $HOME/.cargo/bin $PATH

set -x NO_AT_BRIDGE 1

set -x TERM 'xterm-256color'

alias ll="exa -la --sort changed --icons"
alias open="xdg-open"
alias sbcl="rlwrap sbcl"
alias hy="hy --repl-output-fn=hy.contrib.hy-repr.hy-repr"
alias emacs="env TERM=xterm-24bits /usr/bin/emacs -nw"

# vanilla vim, don't load spacevim
alias vim="vim.nox -u ~/.vimrc-vim.nox"

set -x GEM_HOME ~/.gems
set -x PATH $HOME/.gems/bin $PATH

starship init fish | source
