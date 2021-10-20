fish_default_key_bindings

set -x PATH $HOME/bin $PATH
set -x PATH $HOME/.local/bin $PATH
set -x PATH $HOME/.cargo/bin $PATH
set -x PATH $HOME/.local/bin $PATH
set -x PATH $HOME/.npm/modules/bin $PATH

set -x NO_AT_BRIDGE 1
set -x NPM_CONFIG_PREFIX $HOME/.npm/modules/

set -x TERM 'xterm-256color'
# set -x TERM 'xterm-24bit'

set -x DISPLAY=(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0
set -x LIBGL_ALWAYS_INDIRECT=1

alias sbcl="rlwrap sbcl"
alias hy="hy --repl-output-fn=hy.contrib.hy-repr.hy-repr"

alias ls=exa
alias ll="exa -la -s modified --icons"
alias open="xdg-open"
alias emacs="env TERM=xterm-24bits /usr/bin/emacs"
alias vim="nvim -u ~/.vimrc-vim.nox"

source ~/.keychain/*-fish

starship init fish | source
