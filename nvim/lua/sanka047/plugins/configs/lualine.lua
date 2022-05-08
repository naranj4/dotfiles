--------------------------------------------------------------------------------
-- Lualine Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, lualine = pcall(require, 'lualine')
    if not ok then
        log.error('lualine not available', 'Config')
        return false
    end

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
                left = '╲',
                right = '╱',
            },
            section_separators = {
                left = '',
                right = '',
            },
            disabled_filetypes = {},
            always_divide_middle = true,
        },
        sections = {
            lualine_a = {'mode'},
            lualine_b = {
                {
                    'branch',
                    separator = ' ',
                    fmt = function(str)
                        local limit = 30 -- branch name size limit
                        if str:len() > limit then
                            return str:sub(1, limit - 3) .. '...'
                        else
                            return str
                        end
                    end,
                },
                {
                    'diff',
                    source = function () return vim.b.gitsigns_status_dict end,
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
            lualine_x = {'encoding', 'filetype'},
            lualine_y = {'progress'},
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
        tabline = {
            lualine_a = {
                {
                    'tabs',
                    mode = 2,
                    max_length = vim.o.columns * 2 / 3,
                    tabs_color = {
                        active = 'lualine_a_normal',
                        inactive = 'lualine_a_inactive',
                    },
                    component_separators = {
                        left = '╱',
                        right = '╲',
                    },
                    section_separators = {
                        left = '',
                        right = '',
                    },
                },
            },
            lualine_b = {},
            lualine_c = {},
            lualine_x = {},
            lualine_y = {
                {
                    search_result,
                    component_separators = {
                        left = '╱',
                        right = '╲',
                    },
                    section_separators = {
                        left = '',
                        right = '',
                    },
                },
                {
                    'diagnostics',
                    source = { 'nvim_diagnostic' },
                    sections = { 'error', 'warn', 'info', 'hint' },
                    always_visible = true,
                    colored = true,
                    component_separators = {
                        left = '╱',
                        right = '╲',
                    },
                    section_separators = {
                        left = '',
                        right = '',
                    },
                },
            },
            lualine_z = {
                {
                    'fileformat',
                    component_separators = {
                        left = '╱',
                        right = '╲',
                    },
                    section_separators = {
                        left = '',
                        right = '',
                    },
                },
            },
        },
        extensions = {'quickfix', 'nvim-tree', 'toggleterm'},
    }
    lualine.setup(config)
end

return M
