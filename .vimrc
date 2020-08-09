" vim-plug setup
" install vim-plug automatically
let plugin_folder = '~/.vim'
let vim_config = '~/.vimrc'

" set neovim path
if has('nvim')
    let plugin_folder = '~/.config/nvim'
    let vim_config = '~/.config/nvim/init.vim'
endif

if empty(glob(plugin_folder . '/autoload/plug.vim'))
    let path = plugin_folder . '/autoload/plug.vim'
    silent execute '!curl -fLo ' . path . ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    autocmd VimEnter * PlugInstall --sync | silent execute 'source ' . vim_config
endif

" manage all vim plugins here
call plug#begin(plugin_folder . '/dev-plug')

" color schemes and aesthetics
Plug 'altercation/vim-colors-solarized'
Plug 'cocopon/iceberg.vim'
Plug 'itchyny/lightline.vim'

" File/Directory navigation
" Plug 'ctrlpvim/ctrlp.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'mileszs/ack.vim'

" Comment blocks of code with ease
Plug 'preservim/nerdcommenter'

" Parenthesis surrounding magic
Plug 'machakann/vim-sandwich'
Plug 'jiangmiao/auto-pairs'

" Moving around the screen
Plug 'easymotion/vim-easymotion'

" Git help
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

call plug#end()

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
let g:solarized_termcolors=256
set background=dark
colorscheme solarized

" ESC alternative in insert mode
inoremap kj <esc>
if has('nvim')
    " ESC switches back to normal mode for nvim :terminal
    tnoremap <Esc> <C-\><C-n>
endif

" Switching ; and :
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

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

" move to preview window
nnoremap gp :wincmd<Space>P<CR>

" Show matching braces when cursor is over them
set showmatch

" Highlight search
set hls

" Detect filetype
filetype plugin indent on

" Turn on syntax highlighting.
syntax on

" Visual autocomplete for command menu
set wildmenu

" Highlight current line
set cursorline

" Show command in bottom bar
set showcmd

" Lines wrap instead of continuing off screen
set linebreak

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

" Lightline status bar
let g:lightline = {
    \ 'colorscheme': 'solarized',
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

" This enables relative line numbering mode. With both number and
" relativenumber enabled, the current line shows the true line number, while
" all other lines (above and below) are numbered relative to the current line.
" This is useful because you can tell, at a glance, what count is needed to
" jump up or down to a particular line, by {count}k to go up or {count}j to go
" down.
" set relativenumber

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

" Unbind some useless/annoying default key bindings.
nmap Q <Nop> " 'Q' in normal mode enters Ex mode. You almost never want this.

" Disable audible bell because it's annoying.
set noerrorbells visualbell t_vb=

" Enable mouse support. You should avoid relying on this too much, but it can
" sometimes be convenient.
" set mouse+=a

" Try to prevent bad habits like using the arrow keys for movement. This is
" not the only possible bad habit. For example, holding down the h/j/k/l keys
" for movement, rather than using more efficient movement commands, is also a
" bad habit. The former is enforceable through a .vimrc, while we don't know
" how to prevent the latter.
" Do this in normal mode...
nnoremap <Left>  :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up>    :echoe "Use k"<CR>
nnoremap <Down>  :echoe "Use j"<CR>
" ...and in insert mode
inoremap <Left>  <ESC>:echoe "Use h"<CR>
inoremap <Right> <ESC>:echoe "Use l"<CR>
inoremap <Up>    <ESC>:echoe "Use k"<CR>
inoremap <Down>  <ESC>:echoe "Use j"<CR>

" " Ctrl-P settings:
" let g:ctrlp_map = '<c-p>'
" let g:ctrlp_cmd = 'CtrlP'
"
" " r - the nearest ancestor of current file that contains .git, .hg, etc
" let g:ctrlp_working_path_mode = 'ra'
" let g:ctrlp_root_markers = ['pom.xml', '.p4ignore']
"
" let g:ctrlp_user_command = 'find %s -type f'        " MacOSX/Linux
"
" let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

" Fzf settings
nnoremap <silent> <c-p> :call fzf#run(fzf#wrap({'source': 'ag --hidden --ignore .git -g ""'}))<CR>

" Space and Tab configuration
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" Jump to start and end of line using the home row keys
map H ^
map L $

" (Shift)Tab (de)indents code
" vnoremap <Tab> >
" vnoremap <S-Tab> <

" Open NERDTree automatically on startup
" autocmd vimenter * NERDTree
nnoremap <C-n> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif


" Leader Key
let mapleader = " "


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

" Easymotion settings
let g:EasyMotion_do_mapping = 0 " Disable default mappings

" Turn on case-insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" Easymotion search override
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)

" These `n` & `N` mappings are options. You do not have to map `n` & `N` to EasyMotion.
" Without these mappings, `n` & `N` works fine. (These mappings just provide
" different highlight method and have some other features )
" map  n <Plug>(easymotion-next)
" map  N <Plug>(easymotion-prev)

" Jump to anywhere with 2 chars?
nmap <Leader>s <Plug>(easymotion-s2)

" Ack / Ag
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ack!<Space>
command -nargs=+ Gag Gcd | Ack! <args>
if executable('ag')
    " let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
    let g:ackprg = 'ag --vimgrep'
endif

let g:ack_mappings = {
              \  'v':  '<C-W><CR><C-W>L<C-W>p<C-W>J<C-W>p',
              \ 'gv': '<C-W><CR><C-W>L<C-W>p<C-W>J' }

" Fugitive key bindings
nnoremap <Leader>g :G<Space>

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
