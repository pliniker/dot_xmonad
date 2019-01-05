# set -x RUST_SRC_PATH /home/pliniker/src/3rd/rust/src
set -x PATH $HOME/.cargo/bin $PATH

set -x PATH $HOME/bin $PATH

set -x NO_AT_BRIDGE 1

set -x TERM 'xterm-256color'

alias hy "hy --repl-output-fn=hy.contrib.hy-repr.hy-repr"


set REPO_ROOT ~/.local/lib/python3.6/site-packages/
set fish_function_path $fish_function_path "$REPO_ROOT/powerline/bindings/fish"
powerline-setup
