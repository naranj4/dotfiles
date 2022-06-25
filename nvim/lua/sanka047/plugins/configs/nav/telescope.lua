--------------------------------------------------------------------------------
-- Telescope Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local finder = require('sanka047.utils.finder')
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group
local create_command = require('sanka047.utils.map').create_command

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
                        local width = math.ceil(max_columns * 0.8)
                        if width > 120 then
                            width = 120
                        end
                        return width
                    end,
                    preview_height = function (self, max_columns, max_lines)
                        local height = math.ceil(max_lines * 0.3)
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

    -- load extensions
    local extensions = { 'fzf', 'hop', 'notify', 'harpoon', 'aerial' }
    for _, extension in ipairs(extensions) do
        telescope.load_extension(extension)
    end
end

--------------------------------------------------------------------------------
-- Telescope Keymap
--------------------------------------------------------------------------------
function M.keymap()
    create_command(
        'TRG',
        function (args) finder.search_string(unpack(args.fargs)) end,
        'Ripgrep for string in directory and pipe results to Telescope',
        { nargs = '+' }
    )
    create_command(
        'TFindFiles', -- just so that command completion makes directory suggestions
        function (args)
            local directory = nil
            local skip_git = false
            -- parse args
            for _, v in ipairs(args.fargs) do
                if v == '-u' then
                    skip_git = true
                else
                    -- NOTE: this will overwrite the directory with the most recent value
                    directory = v
                end
            end

            finder.project_files(directory, skip_git)
        end,
        'Find files in a directory (use -u to unrestrict search and ignore .ignore files)',
        { nargs = '+' }
    )
    create_command(
        'TFindContaining',
        function (args) finder.find_files_containing(args.args) end,
        'Find files containing string',
        { nargs = 1 }
    )

    map_group('n', '<leader>f', 'telescope-find')
    map('n', '<leader>ff', 'Find Files', finder.project_files)
    map('n', '<leader>fF', 'Find Files (in dir)', ':TFindFiles<space>')
    map('n', '<leader>fbb', 'Find Buffers', function () require('telescope.builtin').buffers() end)
    map('n', '<leader>fhs', 'Find History', function () require('telescope.builtin').oldfiles() end)

    map('n', '<leader>fbm', 'Find Bookmarks', function () require('telescope.builtin').marks() end)
    map('n', '<leader>fc', 'Find Containing', ':FindContaining<space>')

    map('n', '<leader>fs', 'Find String', ':TelescopeRG<space>')
    map('n', '<leader>fS', 'Find String (Cursor)', function () require('telescope.builtin').grep_string() end)
    map('n', '<leader>fl', 'Live Grep', function () require('telescope.builtin').live_grep() end)
    map('n', '<leader>/', 'Find in Buffer', function () require('telescope.builtin').current_buffer_fuzzy_find() end)

    map('n', '<leader>fhh', 'Find Help Tags', function () require('telescope.builtin').help_tags() end)

    map('n', '<leader>fa', 'Find Aerial', function ()
        require('telescope').extensions.aerial.aerial()
    end)
    map('n', '<leader>fn', 'Find Notifications', function () require('telescope').extensions.notify.notify() end)
    map('n', '<leader>mf', 'Find Marks (Harpoon)', function () require('telescope').extensions.harpoon.marks() end)

    map('n', '<leader><leader>vf', 'Find Neovim Config File', finder.search_dotfiles)
end

return M
