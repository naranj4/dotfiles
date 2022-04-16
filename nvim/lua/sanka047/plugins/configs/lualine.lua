--------------------------------------------------------------------------------
-- Lualine Config
--------------------------------------------------------------------------------
local ok, lualine = pcall(require, 'lualine')
if not ok then
    vim.notify('lualine not available', 'error')
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
            left = '',
            right = '',
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
                separator = { left = '', right = ''},
                diff_color = {
                    -- Same values like general color option can be used here.
                    added    = 'GitSignsAdd',    -- changes diff's added color
                    modified = 'GitSignsChange', -- changes diff's modified color
                    removed  = 'GitSignsDelete', -- changes diff's removed color
                },
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
    tabline = {
        lualine_a = {
            {
                'tabs',
                mode = 2,
                max_length = vim.o.columns,
                tabs_color = {
                    active = 'lualine_a_normal',
                    inactive = 'lualine_a_inactive',
                },
                component_separators = {
                    left = '╱',
                    right = '╲',
                },
                section_separators = {
                    left = '',
                    right = '',
                },
            },
        },
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {
            {
                'diagnostics',
                source = { 'nvim_diagnostic' },
                sections = { 'error', 'warn', 'info', 'hint' },
                separator = { left = '', right = ''},
                always_visible = true,
                diagnostics_color = {
                    error = 'DiagnosticError',
                    warn = 'DiagnosticWarn',
                    info = 'DiagnosticInfo',
                    hint = 'DiagnosticHint',
                },
            },
        },
    },
    extensions = {'quickfix', 'nvim-tree'},
}
lualine.setup(config)
