set mouse=a
set encoding=UTF-8
set cursorline
let mapleader=" "


" More natural pane spliting
set splitbelow
set splitright

" Line Numbering
set number relativenumber

" Indentation
filetype plugin indent on
" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab

" set Vim-specific sequences for RGB colors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

if (has("termguicolors"))
    set termguicolors
endif

" Use status bar even with single buffer
set laststatus=2

syntax on

" Lightline recommended defaults
" -- INSERT -- is unnecessary anymore because the mode information is
"  displayed in the statusline. 
set noshowmode

" COC recommended defaults
" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Vertical split in conflict resolution
set diffopt+=vertical

" Setup characters for whitespace
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:.

set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
set nofoldenable


" Move highlighted stuff up down
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv


" Add line length end indicator
set colorcolumn=88
