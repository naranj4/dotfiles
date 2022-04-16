--------------------------------------------------------------------------------
-- Notify Config
--------------------------------------------------------------------------------
local ok, notify = pcall(require, 'notify')
if not ok then
    vim.notify('notify not available', 'error')
    return false
end

notify.setup({
    stages = "fade",
    render = "default",
})

vim.notify = notify
