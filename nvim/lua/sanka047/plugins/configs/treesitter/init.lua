--------------------------------------------------------------------------------
-- Treesitter Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local create_autocmd = require('sanka047.utils.map').create_autocmd

local M = {}

local ask_install = {}

local function ensure_treesitter_language_installed()
    local parsers = require 'nvim-treesitter.parsers'
    local lang = parsers.get_buf_lang()
    if parsers.get_parser_configs()[lang] and not parsers.has_parser(lang) and ask_install[lang] ~= false then
        vim.schedule_wrap(function()
            vim.ui.select(
                { 'yes', 'no' },
                { prompt = 'Install tree-sitter parsers for ' .. lang .. '? ' },
                function(item)
                    if item == 'yes' then
                        vim.cmd('TSInstall ' .. lang)
                        -- NOTE: for whatever reason, it will reprompt to install after the parser
                        -- has been successfully installed during the session
                        ask_install[lang] = false
                    else
                        log.warn(
                            (
                                'Treesitter parser for ' .. lang .. ' is not installed.\n\n'
                                .. 'If desired, this must be done manually using:\n'
                                .. '```\n:TSInstall ' .. lang .. '\n```'
                            ),
                            'Treesitter'
                        )
                        ask_install[lang] = false
                    end
                end
            )
        end)()
    end
end

local function get_module_config(treesitter_module)
    return require('sanka047.plugins.configs.treesitter.modules.' .. treesitter_module)
end

function M.setup()
    local ok, treesitter_configs = pcall(require, 'nvim-treesitter.configs')
    if not ok then
        log.error('nvim-treesitter not available', 'Config')
        return false
    end

    local treesitter_install = require('nvim-treesitter.install')

    treesitter_configs.setup({
        ensure_installed = {
            'help',
            'json',
            'lua',
            'markdown',
            'python',
            'query',
            'ruby',
            'vim',
            'yaml',
        },
        sync_install = false,
        ignore_install = {},

        -- Treesitter modules
        highlight = get_module_config('highlight').config(),
        indent = { enable = true },
        textobjects = get_module_config('textobjects').config(),
        textsubjects = get_module_config('textsubjects').config(),
        playground = get_module_config('playground').config(),
        query_linter = get_module_config('query-linter').config(),
        context_commentstring = { enable = true },
        autotag = { enable = true },
        endwise = { enable = true },
    })
    treesitter_install.prefer_git = true

    create_autocmd(
        'FileType',
        'Ensure that the TS parser for the filetype is installed',
        {
            pattern = '*',
            callback = ensure_treesitter_language_installed,
        }
    )
end

--------------------------------------------------------------------------------
-- Treesitter Keymap
--------------------------------------------------------------------------------
function M.keymap()
    get_module_config('textobjects').keymap()
    get_module_config('textsubjects').keymap()
    get_module_config('playground').keymap()
end

return M
