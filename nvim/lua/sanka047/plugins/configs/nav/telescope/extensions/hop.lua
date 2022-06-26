--------------------------------------------------------------------------------
-- Hop Extension
--------------------------------------------------------------------------------
local actions = require('telescope.actions')
local custom_actions = require('sanka047.plugins.configs.nav.telescope.actions')

local M = {}

local function hop_qflist(prompt_bufnr)
    local opts = {
        callback = actions.toggle_selection,
        loop_callback = custom_actions.open_in_qflist,
    }
    require('telescope').extensions.hop._hop_loop(prompt_bufnr, opts)
end

local function hop_select(prompt_bufnr)
    local opts = { callback = actions.select_default }
    require('telescope').extensions.hop._hop(prompt_bufnr, opts)
end

function M.config()
    return {
        keys = {
            'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';',
            'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p',
            'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.',
            'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':',
            'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P',
            'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>',
        },
        sign_hl = { 'Title', 'Title' },
        line_hl = { 'Normal', 'Normal' },
        clear_selection_hl = false,
        trace_entry = true,
        reset_selection = true,
    }
end

function M.keymap()
    return {
        i = {
            ['<C-j>'] = hop_select,
            ['<C-Space>'] = hop_qflist,
        },
        n = {
            ['<C-j>'] = hop_select,
            ['<C-Space>'] = hop_qflist,
        },
    }
end

return M
