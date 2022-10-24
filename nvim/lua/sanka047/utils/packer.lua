--------------------------------------------------------------------------------
-- Packer Functions
--------------------------------------------------------------------------------
local M = {}

function M.packer_sync()
    local has_plenary, p_async = pcall(require, 'plenary.async')
    if has_plenary then
        p_async.run(function ()
            local notify = vim.notify
            if type(notify) == 'table' then
                notify.async('Syncing packer. A snapshot will be created.', vim.log.levels.INFO, { title = 'Packer' })
            end
        end)
    end

    local packer = require('packer')

    local snap_shot_time = os.date('!%Y-%m-%dT%TZ')
    packer.snapshot(snap_shot_time)
    packer.sync()
end

return M
