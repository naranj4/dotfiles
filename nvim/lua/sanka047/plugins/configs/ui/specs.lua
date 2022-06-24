--------------------------------------------------------------------------------
-- Specs Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, specs = pcall(require, 'specs')
    if not ok then
        log.error('specs not available', 'Config')
        return false
    end

    specs.setup({
        show_jumps = true,
        min_jump = 2,
        popup = {
            delay_ms = 0,
            inc_ms = 10,
            blend = 20,
            width = 30,
            winhl = 'Search',
            fader = specs.exp_fader,
            resizer = specs.shrink_resizer,
        },
        ignore_filetypes = {
            dashboard = true,
            packer = true,
            lspinfo = true,
        },
        ignore_buftypes = {
            nofile = true,
            prompt = true,
            terminal = true,
        },
    })
end

return M
