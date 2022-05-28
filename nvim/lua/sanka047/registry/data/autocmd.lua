--------------------------------------------------------------------------------
-- Autocmd
--------------------------------------------------------------------------------
local AutoCmd = {}
AutoCmd.__index = AutoCmd

-- Create a new AutoCmd object
-- @param event string: neovim event. See `:h event` for list of possible events
-- @param opts table: see `:h vim.api.nvim_create_autocmd` for valid options
-- @return AutoCmd
function AutoCmd.new(event, opts)
    local cmd = {}
    setmetatable(cmd, AutoCmd)

    cmd.event = event or ''
    cmd.opts = opts or {}

    -- create an autocmd with the appropriate parameters
    cmd.id = vim.api.nvim_create_autocmd(event, opts)

    return cmd
end

-- Removes and clean up the autocmd
-- @param cmd AutoCmd: the autocmd to clean up
function AutoCmd.remove(cmd)
    vim.api.nvim_del_autocmd(cmd.id)
end

-- String representation of an autocmd
-- @param cmd AutoCmd
-- @return string
function AutoCmd.tostring(cmd)
    return (
        'autocmd(' .. cmd.id .. ') ' .. cmd.desc .. ':\n'
        .. '\tevent: ' .. cmd.event .. '\n'
        .. '\topts: ' .. vim.inspect(cmd.opts)
    )
end

return AutoCmd
