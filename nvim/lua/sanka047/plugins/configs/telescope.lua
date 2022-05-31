--------------------------------------------------------------------------------
-- Telescope Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, telescope = pcall(require, 'telescope')
    if not ok then
        log.error('telescope not available', 'Config')
        return false
    end

    local actions = require('telescope.actions')

    local function open_in_qflist(prompt_bufnr)
        actions.smart_send_to_qflist(prompt_bufnr)
        actions.open_qflist(prompt_bufnr)
    end

    local function hop_qflist(prompt_bufnr)
        local opts = {
            callback = actions.toggle_selection,
            loop_callback = open_in_qflist,
        }
        require('telescope').extensions.hop._hop_loop(prompt_bufnr, opts)
    end

    local function hop_select(prompt_bufnr)
        local opts = { callback = actions.select_default }
        require('telescope').extensions.hop._hop(prompt_bufnr, opts)
    end

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
            mappings = {
                i = {
                    ['<C-q>'] = open_in_qflist,
                    ['<ESC>'] = actions.close,

                    ['<C-j>'] = hop_select,
                    ['<C-Space>'] = hop_qflist,
                },
                n = {
                    ['<C-q>'] = open_in_qflist,
                    ['<C-u>'] = actions.results_scrolling_up,
                    ['<C-d>'] = actions.results_scrolling_down,

                    ['<C-j>'] = hop_select,
                    ['<C-Space>'] = hop_qflist,
                }
            },
        },
        pickers = {
            -- Default configuration for builtin pickers goes here:
            -- picker_name = {
            --   picker_config_key = value,
            --   ...
            -- }
            -- Now the picker_config_key will be applied every time you call this
            -- builtin picker
            find_files = {
                -- find_command = {'fd', '--type', 'f'},
                find_command = {'rg', '--ignore', '--hidden', '--files'},
            },
            git_files = {},
            diagnostics = { bufnr = 0 },
            current_buffer_fuzzy_find = {
                layout_strategy = 'vertical',
                layout_config = {
                    prompt_position = 'bottom',
                    height = 0.9,
                    width = function (self, max_columns, max_lines)
                        local width = max_columns * 0.8
                        if width > 120 then
                            width = 120
                        end
                        return width
                    end,
                    preview_height = function (self, max_columns, max_lines)
                        local height = max_lines * 0.3
                        if height < 5 then
                            height = 5
                        elseif height > 15 then
                            height = 15
                        end

                        return height
                    end
                },
            },
        },
        extensions = {
            -- Your extension configuration goes here:
            -- extension_name = {
            --   extension_config_key = value,
            -- }
            -- please take a look at the readme of the extension you want to configure
            fzf = {
                fuzzy = true,
                override_generic_sorter = true,
                override_file_sorter = true,
                case_mode = 'smart_case',
            },
            hop = {
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
            },
        },
    })
    telescope.load_extension('fzf')
    telescope.load_extension('hop')
    telescope.load_extension('notify')
end

return M
