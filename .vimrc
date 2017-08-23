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
Plug 'Raimondi/delimitMate'
Plug 'guns/vim-clojure-static'
Plug 'luochen1990/rainbow'
Plug 'guns/vim-clojure-highlight'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'elmcast/elm-vim'
Plug 'apple/swift', {'rtp': 'utils/vim'}
Plug 'mitsuse/autocomplete-swift'
Plug 'fatih/vim-go'
Plug 'isRuslan/vim-es6'
Plug 'wokalski/autocomplete-flow'
Plug 'flowtype/vim-flow'
Plug 'w0rp/ale'
Plug 'elzr/vim-json'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'jacoborus/tender'
Plug 'posva/vim-vue'
Plug 'othree/es.next.syntax.vim'

" Themes
Plug 'joshdick/onedark.vim'
Plug 'morhetz/gruvbox'

" Add plugins to &runtimepath
call plug#end()

set nocompatible

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread

" Make buffer modifiable
set modifiable

" Enable syntax highlighting
syntax enable

" Set utf8 as standard encoding
set encoding=utf-8

" Color scheme
set background=dark

if $TERM == "xterm-256color" || $TERM == "screen-256color" || $COLORTERM == "gnome-terminal"
  set t_Co=256
endif

" NVIM true color 
set termguicolors

"let g:gruvbox_contrast_dark='hard'
"let g:gruvbox_contrast_light='hard'
"let g:gruvbox_italic             = 1
"let g:gruvbox_italicize_comments = 1
"colo gruvbox

colorscheme onedark 

:highlight LineNr guifg=DarkGrey

" Completion
set omnifunc=syntaxcomplete#Complete
set omnifunc=javascriptcomplete#CompleteJS
:highlight Pmenu ctermbg=238 gui=bold

" Enable omni completion.
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS

" tern
if exists('g:plugs["tern_for_vim"]')
  let g:tern_show_argument_hints = 'on_hold'
  let g:tern_show_signature_in_pum = 1
  autocmd FileType javascript setlocal omnifunc=tern#Complete
endif

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

" Set the vertical split character to +
set fillchars=vert:\│

" Airline Config
let g:airline_powerline_fonts=1
let g:airline_theme='onedark'
let g:airline#extensions#ale#enabled = 1

set wildmenu
set wildmode=list:longest,list:full

" Intend
set smartindent
set tabstop=4 "Space per TAB
set softtabstop=4
set shiftwidth=4
set expandtab "Convert TABs into Spaces

" JS
let javascript_enable_domhtmlcss=1
let g:jsx_ext_required = 1
let g:used_javascript_libs = 'react'
let g:javascript_plugin_flow = 1
let g:flow#showquickfix = 0

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

" Clipboard
set clipboard=unnamed

" Newtree
let g:netrw_liststyle=3

" Set text width
set wrap
"set textwidth=80
set formatoptions=qrn1
set colorcolumn=+1

" Show command
set sc

" Rainbow Parenthese
let g:rainbow_active = 0

set nostartofline

" Use deoplete.
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#ternjs#filetypes = [
                \ 'jsx',
                \ 'javascript.jsx',
                \ 'vue'
                \ ]
let g:deoplete#sources#ternjs#docs = 1
let g:tern#command = ["tern"]
let g:tern#arguments = ["--persistent"]

" Elm
let g:elm_detailed_complete = 1

" Close the documentation window when completion is done
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

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
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

"Split navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Move visual block
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv
" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
