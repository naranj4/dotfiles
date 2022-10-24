--------------------------------------------------------------------------------
-- Noice Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local window = require('sanka047.utils.window')

local M = {}

function M.setup()
    local ok, noice = pcall(require, 'noice')
    if not ok then
        log.error('noice not available', 'Config')
        return false
    end

    noice.setup({
        cmdline = {
            enabled = true,
            view = 'cmdline_popup',
            format = {
                search_down = false,
                search_up = false,
            },
        },
        messages = {
            enabled = true,
        },
        views = {
            cmdline_popup = {
                border = {
                    style = window.border(window.margin.FULL),
                    padding = { 1, 1 },
                    text = { top = '' },
                },
                win_options = {
                    winhighlight = {
                        Normal = 'TelescopePromptNormal',
                        FloatBorder = 'TelescopePromptBorder',
                    },
                },
            },
            split = {
                enter = true,
                win_options = { winhighlight = { Normal = 'Normal' } },
            },
        },
    })
end

return M
