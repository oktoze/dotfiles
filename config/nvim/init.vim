syntax enable
set number
set ts=4
set autoindent
set expandtab
set shiftwidth=4
set showmatch
set foldmethod=indent
set foldlevel=99
set encoding=utf-8
set backupdir=~/.local/share/nvim/backup
set nocompatible
filetype plugin indent on
let python_highlight_all = 1
let g:airline_powerline_fonts = 1
filetype off

call plug#begin('~/.vim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-commentary'
Plug 'easymotion/vim-easymotion'
call plug#end()


" running macro over all visually selected lines
function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

" Keyboard mappings
inoremap <C-j> <ESC>
xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>
xnoremap <C-c> "+y "
