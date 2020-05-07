set nocompatible
set background=dark

syntax on

filetype plugin on
filetype indent on

set autoread
au FocusGained,BufEnter * checktime

set relativenumber
set ruler

set hlsearch
set incsearch
set showmatch

set noerrorbells
set novisualbell

set encoding=utf8

set nobackup
set nowb
set noswapfile

set expandtab

set smarttab
set shiftwidth=4
set tabstop=4

nmap <A-q> :qa<Enter>
