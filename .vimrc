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
    autocmd VimEnter * PlugInstall --sync
    autocmd VimEnter * CocInstall coc-json
    autocmd VimEnter * CocInstall coc-jedi | silent execute 'source ' . vim_config
endif

" manage all vim plugins here
call plug#begin(plugin_folder . '/dev-plug')

" color schemes and aesthetics
Plug 'altercation/vim-colors-solarized'
Plug 'cocopon/iceberg.vim'
Plug 'whatyouhide/vim-gotham'

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

" Conquer of Completion
Plug 'neoclide/coc.nvim', { 'branch': 'release' }

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
" let g:solarized_termcolors=256
" set background=dark
" colorscheme solarized
set termguicolors
colorscheme gotham

" ESC alternative in insert mode
inoremap kj <esc>
if has('nvim')
    " ESC switches back to normal mode for nvim :terminal
    tnoremap kj <C-\><C-n>
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
nnoremap <silent> <ESC> :noh<CR><ESC>

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
function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction

let g:lightline = {
    \ 'colorscheme': 'gotham',
    \ 'active': {
    \   'left': [
    \       [ 'mode', 'paste' ],
    \       [ 'gitbranch', 'cocstatus', 'currentfunc', 'readonly', 'filename', 'modified' ]
    \   ],
    \   'right': [
    \       [ 'lineinfo' ],
    \       [ 'percent' ],
    \       [ 'fileformat', 'fileencoding', 'filetype' ]
    \   ]
    \ },
    \ 'component_function': {
    \   'cocstatus': 'coc#status',
    \   'currentfunc': 'CocCurrentFunction',
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
" Opens a floating buffer to fuzzy search active directory for files
nnoremap <silent> <c-f> :call fzf#run(fzf#wrap({'source': 'ag --hidden --ignore .git -g ""'}))<CR>

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
nnoremap <C-a> :NERDTreeToggle<CR>
" nnoremap <C-f> :NERDTreeFind<CR>
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
" map  / <Plug>(easymotion-sn)
" omap / <Plug>(easymotion-tn)

" These `n` & `N` mappings are options. You do not have to map `n` & `N` to EasyMotion.
" Without these mappings, `n` & `N` works fine. (These mappings just provide
" different highlight method and have some other features )
" map  n <Plug>(easymotion-next)
" map  N <Plug>(easymotion-prev)

" Jump to anywhere with 2 chars?
nmap <Leader>s <Plug>(easymotion-s2)

" Ack / Ag
" Search for something within a file in the directory
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ack!<Space>
command! -nargs=+ Gag Gcd | Ack! <args>
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

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
    inoremap <silent><expr> <c-space> coc#refresh()
else
    inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
    inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f <Plug>(coc-format-selected)
nmap <leader>f <Plug>(coc-format-selected)

augroup mygroup
    autocmd!
    " Setup formatexpr specified filetype(s).
    autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
    " Update signature help on jump placeholder.
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" COMMENTED OUT: because I have no clue what this does
" " Applying codeAction to the selected region.
" " Example: `<leader>aap` for current paragraph
" xmap <leader>a  <Plug>(coc-codeaction-selected)
" nmap <leader>a  <Plug>(coc-codeaction-selected)
"
" " Remap keys for applying codeAction to the current buffer.
" nmap <leader>ac  <Plug>(coc-codeaction)

" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Manage extensions.
nnoremap <silent><nowait> <space>e :<C-u>CocList extensions<cr>
" Show all diagnostics.
nnoremap <silent><nowait> <space>d :<C-u>CocList diagnostics<cr>

" Prevent linters from triggering on EasyMotion
autocmd User EasyMotionPromptBegin silent! CocDisable
autocmd User EasyMotionPromptEnd silent! CocEnable
