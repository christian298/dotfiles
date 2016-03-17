call plug#begin('~/.vim/plugged')

Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'jelera/vim-javascript-syntax'
Plug 'othree/yajs.vim'
Plug 'moll/vim-node'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'othree/html5.vim'
Plug 'mattn/emmet-vim'
Plug 'cakebaker/scss-syntax.vim'
Plug 'hail2u/vim-css3-syntax'
Plug 'nanotech/jellybeans.vim'
Plug 'Raimondi/delimitMate'
Plug 'guns/vim-clojure-static'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'guns/vim-clojure-highlight'
Plug 'junegunn/seoul256.vim'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'Shougo/deoplete.nvim'
Plug 'lambdatoast/elm.vim'
Plug 'chriskempson/tomorrow-theme', {'rtp': 'vim'}
Plug 'w0ng/vim-hybrid'
Plug 'zenorocha/dracula-theme', {'rtp': 'vim'} 
Plug 'mhartington/oceanic-next'
Plug 'apple/swift', {'rtp': 'utils/vim'}
Plug 'fatih/vim-go'

" Add plugins to &runtimepath
call plug#end()

set nocompatible
set encoding=utf-8
scriptencoding utf-8

" Enable filetype plugins
filetype off

set modifiable

syntax enable

" Color scheme
set background=dark

if $TERM == "xterm-256color" || $TERM == "screen-256color" || $COLORTERM == "gnome-terminal"
  set t_Co=256
endif

"colo tomorrow-night-bright
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
"let g:hybrid_custom_term_colors = 0
colo oceanicnext

:highlight LineNr guifg=DarkGrey

" Completion
set omnifunc=syntaxcomplete#Complete
set omnifunc=javascriptcomplete#CompleteJS
:highlight Pmenu ctermbg=238 gui=bold
filetype on
filetype plugin on
filetype indent on
au FileType html,xhtml setl ofu=htmlcomplete#CompleteTags
au FileType css setl ofu=csscomplete#CompleteCSS

" Linenumbers
set number
set relativenumber

" Higlight current line
set cursorline

" Normal backspace delete in insert mode
set backspace=2

" No backup
set nobackup
set nowritebackup
set noswapfile

" Show cursor position all the time
set ruler

" Open splits on right and bottom
set splitbelow
set splitright

" using Source Code Pro
set guifont=Source\ Code\ Pro\ for\ Powerline

" Airline Config
let g:airline_powerline_fonts=1
"let g:airline_theme='powerlineish'
let g:airline_theme='oceanicnext'

set wildmenu
set wildmode=list:longest,list:full

" Intend
set smartindent
set tabstop=4 "Space per TAB
set softtabstop=4
set shiftwidth=4
set expandtab "Convert TABs into Spaces

let javascript_enable_domhtmlcss=1

let g:jsx_ext_required = 0

" Allways display status line
set laststatus=2

" Search
set nohls
set incsearch

" Highlight matching {}()[]
set showmatch
set matchtime=3

" Redraw screen only when needed
set lazyredraw

" CtrlP
let g:ctrlp_working_path_mode=0
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'

" Clipboard
set clipboard=unnamed

" Newtree
let g:netrw_liststyle=3

" Set text width
set wrap
set textwidth=80
set formatoptions=qrn1
set colorcolumn=+1

" Show command
set sc

" Rainbow Parenthese
au VimEnter * RainbowParentheses
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']']]

set nostartofline

" Use deoplete.
let g:deoplete#enable_at_startup = 1

" FZF
if has('nvim')
  let $FZF_DEFAULT_OPTS .= ' --inline-info'
endif

" #########
" Remap
" #########

" Remap leader
:let mapleader =" "

nnoremap <silent> <Leader><Leader> :Files<CR>
nnoremap <silent> <Leader><Enter>  :Buffers<CR>
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
map <C-p> :File<CR>


" NerdTree
map <C-n> :NERDTreeToggle<CR>

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

"Split navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
