--------------------------------------------------------------------------------
-- Neovim Settings
--------------------------------------------------------------------------------
local create_augroup = require('sanka047.utils.map').create_augroup
local create_autocmd = require('sanka047.utils.map').create_autocmd

--------------------------------------------------------------------------------
-- Neovim API Aliases
--------------------------------------------------------------------------------
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
opt.ttimeoutlen = 50

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
opt.relativenumber = true
opt.signcolumn = 'yes:1'

opt.termguicolors = true

opt.mouse = ''

opt.cmdheight = 2
opt.showcmd = true
opt.laststatus = 3

opt.inccommand = 'split'

opt.showmatch = true

opt.list = true
opt.listchars = {tab = ' ‒ ', trail = '•'}

-- highlight on yank
create_augroup('YankHighlight')
create_autocmd(
    'TextYankPost',
    'Highlights yanked text',
    {
        group = 'YankHighlight',
        pattern = '*',
        callback = function () vim.highlight.on_yank({ higroup = 'IncSearch', timeout = 250 }) end,
    }
)

--------------------------------------------------------------------------------
-- Neovide settings
--------------------------------------------------------------------------------
-- TODO: continue to explore Neovide for pair programming potential
if vim.fn.exists('g:neovide') then
    opt.guifont = 'MesloLGS Nerd Font Mono:h18'

    g.neovide_input_macos_alt_is_meta = true
    g.neovide_cursor_vfx_mode = 'sonicboom'
end

--------------------------------------------------------------------------------
-- Netrw
--------------------------------------------------------------------------------
g.netrw_liststyle = 3

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
    'nav.quick-scope',
}

for _, plugin in pairs(plugins) do
    LOAD_CONFIG(plugin)
end
