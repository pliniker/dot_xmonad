set -x PATH $HOME/bin $PATH
set -x PATH $HOME/local/bin $PATH
set -x PATH $HOME/.cargo/bin $PATH
set -x PATH $HOME/.npm/modules/bin $PATH
set -x PATH $HOME/.gems/bin $PATH

set -x NO_AT_BRIDGE 1
set -x NPM_CONFIG_PREFIX $HOME/.npm/modules/
set -x GEM_HOME ~/.gems

set -x TERM 'xterm-256color'

alias sbcl="rlwrap sbcl"
alias hy="hy --repl-output-fn=hy.contrib.hy-repr.hy-repr"

alias ll="exa -la --sort changed --icons"
alias open="xdg-open"

alias emacs="env TERM=xterm-24bits /usr/bin/emacs -nw"
alias vim="vim.nox -u ~/.vimrc-vim.nox"

starship init fish | source
