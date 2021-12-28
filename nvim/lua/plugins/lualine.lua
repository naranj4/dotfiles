--------------------------------------------------------------------------------
-- Lualine Config
--------------------------------------------------------------------------------
local colors = require('material.colors')

local function search_result()
    if vim.v.hlsearch == 0 then return '' end

    local last_search = vim.fn.getreg '/'
    if not last_search or last_search == '' then return '' end

    local search_count = vim.fn.searchcount { maxcount = 9999 }
    return last_search .. '(' .. search_count.current .. '/' .. search_count.total .. ')'
end

local config = {
    options = {
        icons_enabled = true,
        theme = 'auto',
        component_separators = {
            left = ' ',
            right = ' ',
        },
        section_separators = {
            left = '',
            right = '',
        },
        disabled_filetypes = {},
        always_divide_middle = true,
    },
    sections = {
        lualine_a = {'mode'},
        lualine_b = {
            'branch',
            'b:gitsigns_status',
            {
                'diagnostics',
                source = { 'nvim' },
                sections = { 'error' },
                diagnostics_color = { error = { bg = colors.red, fg = colors.white } }
            },
            {
                'diagnostics',
                source = { 'nvim' },
                sections = { 'warn' },
                diagnostics_color = { warn = { bg = colors.orange, fg = colors.white } }
            },
        },
        lualine_c = {
            {
                'filename',
                file_status = true,
                symbols = {
                    modified = '[+]',
                    readonly = '[-]',
                    unnamed = '[No Name]',
                }
            },
            {
                '%w',
                cond = function() return vim.wo.previewwindow end,
            },
            {
                '%q',
                cond = function() return vim.bo.buftype == 'quickfix' end,
            },
        },
        lualine_x = {'encoding', 'fileformat', 'filetype'},
        lualine_y = {search_result, 'progress'},
        lualine_z = {'location'},
    },
    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {'location'},
        lualine_y = {},
        lualine_z = {},
    },
    tabline = {},
    extensions = {'quickfix'},
}
require('lualine').setup(config)
