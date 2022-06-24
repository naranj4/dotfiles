--------------------------------------------------------------------------------
-- Neogen Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, neogen = pcall(require, 'neogen')
    if not ok then
        log.error('neogen not available', 'Config')
        return false
    end

    neogen.setup({
        enabled = true,
        snippet_engine = 'luasnip',
        enable_placeholders = false,
        placeholders_hl = "None", -- weird residue highlighting in Python
        languages = {
            python = {
                template = {
                    annotation_convention = 'numpydoc',
                    numpydoc = require('sanka047.doc_templates.custom_numpydoc'),
                },
            },
        },
    })
end

return M
