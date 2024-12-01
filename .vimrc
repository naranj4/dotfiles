" vim-plug setup
" install vim-plug automatically
let plugin_folder = '~/.vim'
let vim_config = '~/.vimrc'

if empty(glob(plugin_folder . '/autoload/plug.vim'))
    let path = plugin_folder . '/autoload/plug.vim'
    silent execute '!curl -fLo ' . path . ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    autocmd VimEnter * PlugInstall --sync
endif

" manage all vim plugins here
call plug#begin(plugin_folder . '/dev-plug')

" color schemes and aesthetics
Plug 'whatyouhide/vim-gotham'

Plug 'itchyny/lightline.vim'

" File/Directory navigation
" Plug 'ctrlpvim/ctrlp.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'mileszs/ack.vim'

" Comment blocks of code with ease
Plug 'preservim/nerdcommenter'

" Parenthesis surrounding magic
Plug 'machakann/vim-sandwich'
Plug 'jiangmiao/auto-pairs'

" Git help
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

call plug#end()

" Leader Key
let mapleader = " "

" Comments in Vimscript start with a `"`.

" If you open this file in Vim, it'll be syntax highlighted for you.

" Vim is based on Vi. Setting `nocompatible` switches from the default
" Vi-compatibility mode and enables useful Vim functionality. This
" configuration option turns out not to be necessary for the file named
" '~/.vimrc', because Vim automatically enters nocompatible mode if that file
" is present. But we're including it here just in case this config file is
" loaded some other way (e.g. saved as `foo`, and then Vim started with
" `vim -u foo`).
set nocompatible

" Setting background to dark
set termguicolors
colorscheme gotham256

" ESC alternative in insert mode
inoremap kj <esc>

" Window switching/creating magic
function! WinMove(key)
    let t:curwin = winnr()
    exec "wincmd ".a:key
    if (t:curwin == winnr())
        if (match(a:key,'[jk]'))
            wincmd v
        else
            wincmd s
        endif
        exec "wincmd ".a:key
    endif
endfunction

nnoremap <silent> <C-J> :call WinMove('j')<CR>
nnoremap <silent> <C-K> :call WinMove('k')<CR>
nnoremap <silent> <C-L> :call WinMove('l')<CR>
nnoremap <silent> <C-H> :call WinMove('h')<CR>

" Show matching braces when cursor is over them
set showmatch

" Highlight search
set hls
nnoremap <silent> <ESC> :noh<CR><ESC>

" Detect filetype
filetype plugin indent on

" Turn on syntax highlighting.
syntax on

" Visual autocomplete for command menu
set wildmenu

" Highlight current line
set cursorline
set cursorcolumn

" Show command in bottom bar
set showcmd

" Set scroll off to always leave lines at the bottom
set scrolloff=999

" Setting ruler
set ruler

" Show a bar at the 100 char mark
set colorcolumn=100

" Disable the default Vim startup message.
set shortmess+=I

" Show line numbers.
set number
set relativenumber

" Lightline status bar
let g:lightline = {
    \ 'colorscheme': 'gotham',
    \ 'active': {
    \   'left': [
    \       [ 'mode', 'paste' ],
    \       [ 'gitbranch', 'readonly', 'filename', 'modified' ]
    \   ],
    \   'right': [
    \       [ 'lineinfo' ],
    \       [ 'percent' ],
    \       [ 'fileformat', 'fileencoding', 'filetype' ]
    \   ]
    \ },
    \ 'component_function': {
    \   'gitbranch': 'FugitiveHead'
    \ },
    \ }

" Always show the status line at the bottom, even if you only have one window open.
set laststatus=2

" The backspace key has slightly unintuitive behavior by default. For example,
" by default, you can't backspace before the insertion point set with 'i'.
" This configuration makes backspace behave more reasonably, in that you can
" backspace over anything.
set backspace=indent,eol,start

" By default, Vim doesn't let you hide a buffer (i.e. have a buffer that isn't
" shown in any window) that has unsaved changes. This is to prevent you from "
" forgetting about unsaved changes and then quitting e.g. via `:qa!`. We find
" hidden buffers helpful enough to disable this protection. See `:help hidden`
" for more information on this.
set hidden

" This setting makes search case-insensitive when all characters in the string
" being searched are lowercase. However, the search becomes case-sensitive if
" it contains any capital letters. This makes searching more convenient.
set ignorecase
set smartcase

" Enable searching as you type, rather than waiting till you press enter.
set incsearch

" Close window
nmap Q <C-w>c

" Disable audible bell because it's annoying.
set noerrorbells visualbell t_vb=

" Enable mouse support. You should avoid relying on this too much, but it can
" sometimes be convenient.
" set mouse+=a

" Fzf settings
" Opens a floating buffer to fuzzy search active directory for files
command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview({'source': 'ag --hidden --ignore .git -g ""'}), <bang>0)
" nnoremap <silent> <c-f> :call fzf#run(fzf#wrap({'source': 'ag --hidden --ignore .git -g ""'}))<CR>
nnoremap <silent> <leader>ff :Files<CR>

" Space and Tab configuration
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" Jump to start and end of line using the home row keys
map H ^
map L $

" vim-sandwich settings
let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)
let g:sandwich#recipes += [
    \   {
    \       'buns'        : ['{', '}'],
    \       'motionwise'  : ['line'],
    \       'kind'        : ['add'],
    \       'linewise'    : 1,
    \       'command'     : ["'[+1,']-1normal! >>"],
    \   },
    \   {
    \       'buns'        : ['{', '}'],
    \       'motionwise'  : ['line'],
    \       'kind'        : ['delete'],
    \       'linewise'    : 1,
    \       'command'     : ["'[,']normal! <<"],
    \   },
    \   {
    \       'buns'        : ['(', ')'],
    \       'motionwise'  : ['line'],
    \       'kind'        : ['add'],
    \       'linewise'    : 1,
    \       'command'     : ["'[+1,']-1normal! >>"],
    \   },
    \   {
    \       'buns'        : ['(', ')'],
    \       'motionwise'  : ['line'],
    \       'kind'        : ['delete'],
    \       'linewise'    : 1,
    \       'command'     : ["'[,']normal! <<"],
    \   },
    \   {
    \       'buns'        : ['[', ']'],
    \       'motionwise'  : ['line'],
    \       'kind'        : ['add'],
    \       'linewise'    : 1,
    \       'command'     : ["'[+1,']-1normal! >>"],
    \   },
    \   {
    \       'buns'        : ['[', ']'],
    \       'motionwise'  : ['line'],
    \       'kind'        : ['delete'],
    \       'linewise'    : 1,
    \       'command'     : ["'[,']normal! <<"],
    \   },
    \ ]

" Ack / Ag
" Search for something within a file in the directory
cnoreabbrev Ack Ack!
nnoremap <leader>FS :Ack!<Space>
nnoremap <leader>fs :Ack!<CR>
xnoremap <leader>fs y:Ack! <C-r>=fnameescape(@")<CR><CR>
command! -nargs=+ Gag Gcd | Ack! <args>
if executable('ag')
    " let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
    let g:ackprg = 'ag --vimgrep'
endif

let g:ack_mappings = {
              \  'v':  '<C-W><CR><C-W>L<C-W>p<C-W>J<C-W>p',
              \ 'gv': '<C-W><CR><C-W>L<C-W>p<C-W>J' }

" GitGutter key bindings
set signcolumn=yes
" Fix sign highlighting
highlight clear SignColumn
highlight GitGutterAdd ctermfg=darkgreen guifg=darkgreen
highlight GitGutterChange ctermfg=yellow guifg=darkyellow
highlight GitGutterDelete ctermfg=red guifg=darkred
highlight GitGutterChangeDelete ctermfg=yellow guifg=darkyellow

" NERDCommenter settings
let g:NERDSpaceDelims = 1  " add a space after comment delims
let g:NERDCompactSexyComs = 1
let g:NERDDefaultAlign = 'left'
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1
let g:NERDToggleCheckAllLines = 1

" CoC settings
" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c
