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
Plug 'jreybert/vimagit'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-commentary'
Plug 'easymotion/vim-easymotion'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'kien/ctrlp.vim'
Plug 'hrsh7th/nvim-compe'
Plug 'dracula/vim'
call plug#end()

" theme
colorscheme dracula

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

lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = { "c", "rust" },  -- list of language that will be disabled
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}
EOF
