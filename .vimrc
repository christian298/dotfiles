call plug#begin('~/.local/share/nvim/plugged')

Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}

Plug 'Shougo/denite.nvim'

Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'othree/html5.vim'
Plug 'mattn/emmet-vim'
Plug 'prettier/vim-prettier', {
  \ 'do': 'npm install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue'] }

" CSS
Plug 'cakebaker/scss-syntax.vim'
Plug 'hail2u/vim-css3-syntax'
Plug 'othree/csscomplete.vim'
Plug 'JulesWang/css.vim'

" JS
Plug 'chemzqm/vim-jsx-improve'
Plug 'jparise/vim-graphql'
Plug 'elzr/vim-json'
Plug 'posva/vim-vue'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'HerringtonDarkholme/yats.vim'

Plug 'Raimondi/delimitMate'
Plug 'luochen1990/rainbow'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

Plug 'elmcast/elm-vim'
Plug 'apple/swift', {'rtp': 'utils/vim'}
Plug 'fatih/vim-go'
Plug 'roxma/nvim-yarp'
Plug 'mitsuse/autocomplete-swift'

" Themes
Plug 'joshdick/onedark.vim'
Plug 'morhetz/gruvbox'
Plug 'chriskempson/base16-vim'

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

colorscheme onedark 

:highlight LineNr guifg=DarkGrey

" Completion
set omnifunc=syntaxcomplete#Complete
set omnifunc=javascriptcomplete#CompleteJS
:highlight Pmenu ctermbg=238 gui=bold

" Enable omni completion.
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
augroup FiletypeGroup
    autocmd!
    au BufNewFile,BufRead *.jsx set filetype=javascript.jsx
augroup END

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
let g:jsx_ext_required = 0
let g:used_javascript_libs = 'react'
let g:javascript_plugin_flow = 1


" Required for operations modifying multiple buffers like rename.
set hidden

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
let g:rainbow_active = 1

set nostartofline

" Elm
let g:elm_detailed_complete = 1

" Close the documentation window when completion is done
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" === Denite setup ==="
" Use ripgrep for searching current directory for files
" By default, ripgrep will respect rules in .gitignore
"   --files: Print each file that would be searched (but don't search)
"   --glob:  Include or exclues files for searching that match the given glob
"            (aka ignore .git files)
"
call denite#custom#var('file/rec', 'command', ['rg', '--files', '--glob', '!.git'])

" Use ripgrep in place of "grep"
call denite#custom#var('grep', 'command', ['rg'])

" Custom options for ripgrep
"   --vimgrep:  Show results with every match on it's own line
"   --hidden:   Search hidden directories and files
"   --heading:  Show the file name above clusters of matches from each file
"   --S:        Search case insensitively if the pattern is all lowercase
call denite#custom#var('grep', 'default_opts', ['--hidden', '--vimgrep', '--heading', '-S'])

" Recommended defaults for ripgrep via Denite docs
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])
" Custom options for Denite
"   auto_resize             - Auto resize the Denite window height automatically.
"   prompt                  - Customize denite prompt
"   direction               - Specify Denite window direction as directly below current pane
"   winminheight            - Specify min height for Denite window
"   highlight_mode_insert   - Specify h1-CursorLine in insert mode
"   prompt_highlight        - Specify color of prompt
"   highlight_matched_char  - Matched characters highlight
"   highlight_matched_range - matched range highlight
let s:denite_options = {'default' : {
\ 'auto_resize': 1,
\ 'direction': 'rightbelow',
\ 'winminheight': '10',
\ 'highlight_mode_insert': 'Visual',
\ 'highlight_mode_normal': 'Visual',
\ 'prompt_highlight': 'Function',
\ 'highlight_matched_char': 'Function',
\ 'highlight_matched_range': 'Normal'
\ }}

" #########
" Remap
" #########

" Remap leader
:let mapleader =" "

" NerdTree
map <C-n> :NERDTreeToggle<CR>
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let g:NERDTreeShowHidden = 1
let g:NERDTreeIgnore = ['^\.DS_Store$', '^tags$', '\.git$[[dir]]', '\.idea$[[dir]]', '\.sass-cache$']

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

" === Denite shorcuts === "
"   ;         - Browser currently open buffers
"   <leader>t - Browse list of files in current directory
"   <leader>g - Search current directory for occurences of given term and
"   close window if no results
"   <leader>j - Search current directory for occurences of word under cursor
nmap ; :Denite buffer -split=floating -winrow=1<CR>
nmap <leader>t :Denite file/rec -split=floating -winrow=1<CR>
nnoremap <leader>g :<C-u>Denite grep:. -no-empty -mode=normal<CR>
nnoremap <leader>j :<C-u>DeniteCursorWord grep:. -mode=normal<CR>

" === coc.nvim === "
nmap <silent> <leader>dd <Plug>(coc-definition)
nmap <silent> <leader>dr <Plug>(coc-references)
nmap <silent> <leader>dj <Plug>(coc-implementation)
