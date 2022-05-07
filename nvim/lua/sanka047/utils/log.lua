--------------------------------------------------------------------------------
-- Mapping Functions
--------------------------------------------------------------------------------
local M = {}

function M.log(msg, hl, name)
    name = name or "Neovim"
    hl = hl or "Todo"
    vim.api.nvim_echo({ { name .. ": ", hl }, { msg } }, true, {})
end

function M.debug(msg, name)
    vim.notify(msg, vim.log.levels.DEBUG, { title = name })
end

function M.info(msg, name)
    vim.notify(msg, vim.log.levels.INFO, { title = name })
end

function M.warn(msg, name)
    vim.notify(msg, vim.log.levels.WARN, { title = name })
end

function M.error(msg, name)
    vim.notify(msg, vim.log.levels.ERROR, { title = name })
end

return M
