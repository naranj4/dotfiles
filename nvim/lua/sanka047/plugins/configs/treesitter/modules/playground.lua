--------------------------------------------------------------------------------
-- Playground Config
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.config()
    return {
        enable = true,
        disable = {},
        updatetime = 50,
        persist_queries = false,
        keybindings = {
            toggle_query_editor = 'o',
            toggle_hl_groups = 'i',
            toggle_injected_languages = 't',
            toggle_anonymous_nodes = 'a',
            toggle_language_display = 'I',
            focus_language = 'f',
            unfocus_language = 'F',
            update = 'R',
            goto_node = '<cr>',
            show_help = '?',
        }
    }
end

function M.keymap()
    map('n', '<leader><leader>ttp', 'TSPlayground', '<CMD>TSPlaygroundToggle<CR>')
end

return M
