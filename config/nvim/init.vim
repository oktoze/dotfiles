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
set completeopt=menuone,noselect

filetype plugin indent on
let python_highlight_all = 1
let g:airline_powerline_fonts = 1
filetype off

call plug#begin('~/.vim/plugged')
Plug 'neovim/nvim-lspconfig'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-commentary'
Plug 'easymotion/vim-easymotion'
Plug 'kien/ctrlp.vim'
Plug 'hrsh7th/nvim-compe'
call plug#end()

" running macro over all visually selected lines
function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

" Keyboard mappings
inoremap jk <ESC>
xnoremap jk <ESC>
xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>
xnoremap <C-c> "+y "
xnoremap <C-i> :norm I 
xnoremap <C-a> :norm A

lua << EOF
require'lspconfig'.pyright.setup{}
require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  resolve_timeout = 800;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = {
    border = { '', '' ,'', ' ', '', '', '', ' ' }, -- the border option is the same as `|help nvim_open_win|`
    winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
    max_width = 120,
    min_width = 60,
    max_height = math.floor(vim.o.lines * 0.3),
    min_height = 1,
  };

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = true;
    ultisnips = true;
    luasnip = true;
  };
}
EOF
