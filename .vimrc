set nocompatible

" Enable filetype plugins
filetype off

set rtp+=~/.nvim/bundle/Vundle.vim
call vundle#begin()

Plugin 'ctrlp.vim'
Plugin 'bling/vim-airline'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'marijnh/tern_for_vim'
Plugin 'moll/vim-node'
Plugin 'othree/javascript-libraries-syntax.vim'
Plugin 'othree/html5.vim'
Plugin 'mattn/emmet-vim'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'nanotech/jellybeans.vim'
Plugin 'chriskempson/base16-vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'groenewege/vim-less'
Plugin 'Raimondi/delimitMate'
Plugin 'tpope/vim-fireplace'
Plugin 'guns/vim-clojure-static'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'guns/vim-clojure-highlight'
Plugin 'junegunn/seoul256.vim'

call vundle#end()

syntax enable

" Color scheme
set background=dark

if $TERM == "xterm-256color" || $TERM == "screen-256color" || $COLORTERM == "gnome-terminal"
  set t_Co=256
endif

"colorscheme hybrid
"colorscheme jellybeans
"colorscheme base16-tomorrow
"let g:jellybeans_use_lowcolor_black = 0

"let g:solarized_termcolors=256
"colorscheme solarized
let g:seoul256_background = 234
colo seoul256


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

set encoding=utf-8

" using Source Code Pro
set guifont=Source\ Code\ Pro\ for\ Powerline

" Airline Config
let g:airline_powerline_fonts=1 

set wildmenu
set wildmode=list:longest,list:full

" Intend
set smartindent
set tabstop=4 "Space per TAB
set softtabstop=4
set shiftwidth=4
set expandtab "Convert TABs into Spaces

let javascript_enable_domhtmlcss=1 

" Allways display status line
set laststatus=2

set clipboard=unnamed

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

" Netre
let g:netrw_liststyle=3

" Set text width
set wrap
set textwidth=80
set formatoptions=qrn1
set colorcolumn=+1

" Show command
set sc

" Rainbow Parenthese
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" Remap leader
:let mapleader ="ä"

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

