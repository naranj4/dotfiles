--------------------------------------------------------------------------------
-- Telescope Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local window = require('sanka047.utils.window')

local M = {}

local function req_telescope(submodule)
    return require('sanka047.plugins.nav.telescope.' .. submodule)
end

local function translate_borderchars_for_telescope(borderchars)
    return {
        borderchars[2],
        borderchars[4],
        borderchars[6],
        borderchars[8],
        borderchars[1],
        borderchars[3],
        borderchars[5],
        borderchars[7],
    }
end

local function get_default_preview_width(_, max_columns, _)
    local width = math.ceil(max_columns * 0.5)
    if width > 100 then
        width = 100
    end
    return width
end

function M.setup()
    local ok, telescope = pcall(require, 'telescope')
    if not ok then
        log.error('telescope not available', 'Config')
        return false
    end

    local actions = require('telescope.actions')
    local custom_actions = require('sanka047.plugins.nav.telescope.actions')

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
            layout_config = {
                prompt_position = 'top',
                width = 0.8,
                height = 0.8,
                preview_cutoff = 120,

                -- layout specific
                center = { preview_cutoff = 40 },
                bottom_pane = { preview_width = get_default_preview_width },
                horizontal = { preview_width = get_default_preview_width },
                vertical = { preview_width = get_default_preview_width },
            },
            borderchars = translate_borderchars_for_telescope(window.border(window.margin.HALF)),
            sorting_strategy = 'ascending',
            prompt_prefix = '  ',
            entry_prefix = '   ',
            selection_caret = '  ',
            multi_icon = '  ',
            path_display = { 'truncate' },
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
    local extensions = { 'fzf', 'hop', 'notify', 'harpoon' }
    for _, extension in ipairs(extensions) do
        telescope.load_extension(extension)
    end
end

return M
