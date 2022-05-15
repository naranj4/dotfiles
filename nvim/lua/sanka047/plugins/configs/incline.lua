--------------------------------------------------------------------------------
-- Incline Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, incline = pcall(require, 'incline')
    if not ok then
        log.error('incline not available', 'Config')
        return false
    end

    incline.setup({
        render = function (props)
            local bufname = vim.api.nvim_buf_get_name(props.buf)
            local res = bufname ~= '' and vim.fn.fnamemodify(bufname, ':t') or '[No Name]'
            if vim.api.nvim_buf_get_option(props.buf, 'modified') then
                res = res .. ' [+]'
            end

            local has_devicons, devicons = pcall(require, 'nvim-web-devicons')
            if has_devicons then
                local icon = devicons.get_icon(bufname, vim.bo[props.buf].filetype)
                if icon then
                    res = icon .. ' ' .. res
                end
            end

            return res
        end,
        window = {
            placement = {
                vertical = 'top',
                horizontal = 'right',
            },
            margin = {
                vertical = 0,
                horizontal = 1,
            },
            winhighlight = {
                active = { Normal = 'InclineNormal' },
                inactive = { Normal = 'InclineNormalNC' },
            },
            padding = 2,
        },
    })
end

return M
