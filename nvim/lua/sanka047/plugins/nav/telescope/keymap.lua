--------------------------------------------------------------------------------
-- Telescope Keymap
--------------------------------------------------------------------------------
local finder = require('sanka047.utils.finder')
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group
local create_command = require('sanka047.utils.map').create_command

local M = {}

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
    map_group('n', '<leader>F', 'telescope-find-cmd')

    map('n', '<leader>FT', 'Telescope', ':Telescope<space>')

    -- find files
    map('n', '<leader>ff', 'Find Files', finder.project_files)
    map('n', '<leader>FF', 'Find Files (in dir)', ':TFindFiles<space>')
    map('n', '<leader>fb', 'Find Buffers', function () require('telescope.builtin').buffers() end)
    map('n', '<leader>fhs', 'Find History', function () require('telescope.builtin').oldfiles() end)

    -- find word/string
    map('n', '<leader>fs', 'Find String (Cursor)', function ()
        require('telescope.builtin').grep_string()
    end)
    map('n', '<leader>FS', 'Find String', ':TelescopeRG<space>')
    map('n', '<leader>FC', 'Find Containing', ':FindContaining<space>')
    map('n', '<leader>fl', 'Live Grep', function () require('telescope.builtin').live_grep() end)
    map('n', '<leader>/', 'Find in Buffer', function ()
        require('telescope.builtin').current_buffer_fuzzy_find()
    end)

    -- extensions
    map('n', '<leader>fa', 'Find Aerial', function ()
        require('telescope').extensions.aerial.aerial()
    end)
    map('n', '<leader>fn', 'Find Notifications', function ()
        require('telescope').extensions.notify.notify()
    end)
    map('n', '<leader>mf', 'Find Marks (Harpoon)', function ()
        require('telescope').extensions.harpoon.marks()
    end)

    -- search config
    map('n', '<leader><leader>vf', 'Find Neovim Config File', finder.search_dotfiles)

    -- search help
    map('n', '<leader>fhh', 'Find Help Tags', function ()
        require('telescope.builtin').help_tags()
    end)
end

return M
