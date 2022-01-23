--------------------------------------------------------------------------------
-- Load Plugins
--------------------------------------------------------------------------------
-- disable unused builtins
local builtins = {
    -- 'netrw',
    -- 'netrwPlugin',
    -- 'netrwSettings',
    -- 'netrwFileHandlers',
}

for _, plugin in pairs(builtins) do
    vim.g['loaded_' .. plugin] = 1
end

require('sanka047.plugins.init')
