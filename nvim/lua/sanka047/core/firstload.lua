--------------------------------------------------------------------------------
-- First Load Actions
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local lazy_path = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
vim.opt.rtp:prepend(lazy_path)

local function download_lazy()
    log.warn('Plugin manager (lazy) is not installed', 'Plugins')

    vim.fn.delete(lazy_path, 'rf')
    vim.fn.mkdir(lazy_path, 'p')

    local out = vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--depth",
        "1",
        lazy_path,
    })

    log.info(out, 'Plugins')
    log.info('Downloading lazy.nvim...', 'Plugins')
    log.warn("( You'll need to restart now )", 'Plugins')
end

if not pcall(require, 'lazy') then
    download_lazy()
    return true
end

return false
