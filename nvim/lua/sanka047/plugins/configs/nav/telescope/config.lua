--------------------------------------------------------------------------------
-- Telescope Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

local function req_telescope(submodule)
    return require('sanka047.plugins.configs.nav.telescope.' .. submodule)
end

function M.setup()
    local ok, telescope = pcall(require, 'telescope')
    if not ok then
        log.error('telescope not available', 'Config')
        return false
    end

    local actions = require('telescope.actions')
    local custom_actions = require('sanka047.plugins.configs.nav.telescope.actions')

    -- Mappings for all extensions
    local mappings = {
        i = {
            ['<C-q>'] = custom_actions.open_in_qflist,
            ['<ESC>'] = actions.close,
        },
        n = {
            ['<C-q>'] = custom_actions.open_in_qflist,
            ['<C-u>'] = actions.results_scrolling_up,
            ['<C-d>'] = actions.results_scrolling_down,
        },
    }
    mappings = vim.tbl_deep_extend('error', mappings, req_telescope('extensions.hop').keymap())

    telescope.setup({
        defaults = {
            -- Default configuration for telescope goes here:
            -- config_key = value,
            vimgrep_arguments = {
                'rg',
                '--color=never',
                '--no-heading',
                '--with-filename',
                '--line-number',
                '--column',
                '--smart-case',
                '--trim',
            },
            mappings = mappings,
        },
        pickers = {
            find_files = {
                -- find_command = {'fd', '--type', 'f'},
                find_command = {'rg', '--ignore', '--hidden', '--files'},
            },
            git_files = {},
            diagnostics = { bufnr = 0 },
            current_buffer_fuzzy_find = req_telescope('pickers.current_buffer_fuzzy_find'),
        },
        extensions = {
            fzf = req_telescope('extensions.fzf').config(),
            hop = req_telescope('extensions.hop').config(),
        },
    })

    -- load extensions
    local extensions = { 'fzf', 'hop', 'notify', 'harpoon', 'aerial' }
    for _, extension in ipairs(extensions) do
        telescope.load_extension(extension)
    end
end

return M
