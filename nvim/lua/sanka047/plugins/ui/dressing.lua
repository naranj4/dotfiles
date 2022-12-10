--------------------------------------------------------------------------------
-- Dressing Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local window = require('sanka047.utils.window')

local M = {}

function M.setup()
    local ok, dressing = pcall(require, 'dressing')
    if not ok then
        log.error('dressing not available', 'Config')
        return false
    end

    dressing.setup({
        input = {
            enabled = true,
            insert_only = true,
            prompt_align = 'right',
            win_options = {
                winblend = 0,
                winhighlight = 'NormalFloat:DressingNormal,FloatBorder:DressingBorder',
            },
            override = function (conf)
                conf['border'] = window.border(window.margin.FULL)
            end
        },
        select = {
            enabled = true,
            backend = { "telescope", "builtin" },
            trim_prompt = true,
            winblend = 0,
        },
    })
end

return M
