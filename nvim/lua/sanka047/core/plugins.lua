--------------------------------------------------------------------------------
-- Load Plugins
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local registry = require('sanka047.registry')
local firstload = require('sanka047.core.firstload')

if firstload then
    require('sanka047.utils.map').create_autocmd(
        'User',
        'Notify user to reload after bootstrap',
        {
            pattern = 'LazyDone',
            callback = function ()
                print('---------------------------------------------------')
                print('Plugin manager (lazy) has finished bootstrapping.')
                print('Plugins have been installed.')
                print('Please restart neovim.')
                print('---------------------------------------------------')
            end,
            once = true,
        }
    )
end

local lazy = require('lazy')
local window = require('sanka047.utils.window')

--------------------------------------------------------------------------------
-- Lazy loading helpers
--------------------------------------------------------------------------------
local function _lazy_load_condition(metadata)
    log.debug('Metadata: ' .. vim.inspect(metadata))
    local ignore_filetypes = {
        TelescopePrompt = true,
        TelescopeResults = true,
        NvimTree = true,
        DressingInput = true,
        alpha = true,
    }

    local ft = vim.bo[metadata.buf].filetype
    if ignore_filetypes[ft] then
        log.debug('Not lazy loading plugins in filetype `' .. ft .. '`')
        return false
    end

    log.debug('Lazy loading plugins in filetype `' .. ft .. '`')
    return true
end

local function _lazy_load_is_complete(metadata)
    log.debug('Metadata: ' .. vim.inspect(metadata))
    if metadata.num_successes > 0 then
        log.debug('Removing lazy load command for ' .. metadata.key)
        return true
    end

    return false
end

local function _lazy_load_plugin(metadata)
    log.debug('Metadata: ' .. vim.inspect(metadata))

    local ft = vim.bo[metadata.buf].filetype
    vim.schedule(function ()
        log.debug('Lazy loading ' .. metadata.key .. ' in filetype: `' .. ft .. '`')
        lazy.load({ plugins = metadata.key })
    end)
end

--------------------------------------------------------------------------------
-- Lazy loading
--------------------------------------------------------------------------------
local function lazy_load_on_event(event, plugin)
    registry.register('LazyLoad', event, plugin, {
        callback = _lazy_load_plugin,
        condition = _lazy_load_condition,
        is_complete = _lazy_load_is_complete,
    })
end

-- List of plugins lazy loaded on an event
local lazy_loaded = {
    ['nvim-autopairs'] = 'InsertEnter',
    ['nvim-cmp'] = 'BufRead',
}
for plugin, event in pairs(lazy_loaded) do
    lazy_load_on_event(event, plugin)
end

lazy.setup('sanka047.plugins', {
    ui = { border = window.border(window.margin.FULL) },
    diff = { cmd = 'diffview.nvim' },
})
