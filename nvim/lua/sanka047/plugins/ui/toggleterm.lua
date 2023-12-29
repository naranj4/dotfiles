--------------------------------------------------------------------------------
-- Toggleterm Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local window = require('sanka047.utils.window')
local map = require('sanka047.utils.map').map

local M = {}

function M.setup()
    local ok, toggleterm = pcall(require, 'toggleterm')
    if not ok then
        log.error('toggleterm not available', 'Config')
        return false
    end

    toggleterm.setup({
        size = function (term)
            if term.direction == 'horizontal' then
                return vim.o.lines * 0.3
            elseif term.direction == 'vertical' then
                return vim.o.columns * 0.4
            end
        end,
        hide_numbers = true,
        highlights = {
            NormalFloat = { link = 'ToggletermNormal' },
            FloatBorder = { link = 'ToggletermBorder' },
        },
        shade_terminals = false,
        start_in_insert = true,
        insert_mappings = false, -- whether or not the open mapping applies in insert mode
        terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
        persist_size = true,
        direction = 'float',
        close_on_exit = true, -- close the terminal window when the process exits
        shell = vim.o.shell, -- change the default shell
        -- This field is only relevant if direction is set to 'float'
        float_opts = {
            -- The border key is *almost* the same as 'nvim_open_win'
            -- see :h nvim_open_win for details on borders however
            -- the 'curved' border is a custom border type
            -- not natively supported but implemented in this plugin.
            border = window.border(window.margin.FULL),
            width = function () return math.ceil(0.8 * vim.o.columns) end,
            height = function () return math.ceil(0.8 * vim.o.lines) end,
            winblend = 0,
        }
    })
end

--------------------------------------------------------------------------------
-- Toggleterm Keymap
--------------------------------------------------------------------------------
function M.keymap()
    local ok, _ = pcall(require, 'toggleterm')
    if not ok then
        log.error('toggleterm not available', 'Keymap')
        return false
    end

    local Terminal = require('toggleterm.terminal').Terminal

    -- simple zsh terminal
    local zsh = Terminal:new({
        direction = 'float',
        hidden = true,
    })

    --------------------------------------------------------------------------------
    -- Toggleterm Keymap
    --------------------------------------------------------------------------------
    map('nt', '<M-i>', 'Toggle ZSH', function () zsh:toggle() end)
end

return M
