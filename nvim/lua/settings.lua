--------------------------------------------------------------------------------
-- Neovim Settings
--------------------------------------------------------------------------------
_G.MUtils = {}  -- global object

--------------------------------------------------------------------------------
-- Neovim API Aliases
--------------------------------------------------------------------------------
local cmd = vim.cmd                 -- execute vim commands
local exec = vim.api.nvim_exec      -- execute vimscript
local fn = vim.fn                   -- call vim functions
local g = vim.g                     -- global variables
local opt = vim.opt                 -- global/buffer/windows-scoped options

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------
opt.compatible = false
opt.hidden = true
opt.errorbells = false
opt.visualbell = false
opt.shortmess:append('I')
opt.confirm = true

g.mapleader = ' '  -- use space as leader

--------------------------------------------------------------------------------
-- UI
--------------------------------------------------------------------------------
opt.linebreak = true
opt.scrolloff = 999
opt.ruler = true
opt.cursorline = true
opt.cursorcolumn = true
opt.colorcolumn = '100'
opt.number = true
opt.signcolumn = 'yes:1'

opt.cmdheight = 2
opt.showcmd = true
opt.laststatus = 2

opt.showmatch = true

opt.list = true
opt.listchars = {tab = '▶ ', trail = '•'}

-- highlight on yank
exec([[
    augroup YankHighlight
        autocmd!
        autocmd TextYankPost * silent! lua vim.highlight.on_yank{higroup="IncSearch", timeout=700}
    augroup end
]], false)

--------------------------------------------------------------------------------
-- Search and Autocompletion
--------------------------------------------------------------------------------
opt.ignorecase = true
opt.smartcase = true

opt.wildmenu = true
opt.wildmode = {'longest:full', 'full'}

opt.hlsearch = true
opt.incsearch = true

--------------------------------------------------------------------------------
-- Backspace, Tab and Space
--------------------------------------------------------------------------------
opt.backspace = {'indent', 'eol', 'start'}

opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true

opt.joinspaces = false
