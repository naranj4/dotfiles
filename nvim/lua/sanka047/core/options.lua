--------------------------------------------------------------------------------
-- Neovim Settings
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Neovim API Aliases
--------------------------------------------------------------------------------
local cmd = vim.cmd                 -- execute vim commands
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

opt.directory = vim.fn.stdpath('config') .. '/.swap//'

g.mapleader = ' '  -- use space as leader

--------------------------------------------------------------------------------
-- Timeout (for terminal keys)
--------------------------------------------------------------------------------
opt.ttimeout = true
opt.ttimeoutlen = 100

--------------------------------------------------------------------------------
-- UI
--------------------------------------------------------------------------------
opt.wrap = false
opt.linebreak = false
opt.sidescroll = 5
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
opt.listchars = {tab = '‒‒▶', trail = '•'}

-- highlight on yank
cmd([[
    augroup YankHighlight
        autocmd!
        autocmd TextYankPost * silent! lua vim.highlight.on_yank{higroup="IncSearch", timeout=700}
    augroup end
]])

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

--------------------------------------------------------------------------------
-- Formatting Options
--------------------------------------------------------------------------------
opt.textwidth = 100
opt.formatoptions:remove('t')

--------------------------------------------------------------------------------
-- Plugin Specific Config (must be specified at startup)
--------------------------------------------------------------------------------
local plugins = {
    'quick-scope',
}

for _, plugin in pairs(plugins) do
    LOAD_CONFIG(plugin)
end
