--------------------------------------------------------------------------------
-- Neogen Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

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

--------------------------------------------------------------------------------
-- Neogen Keymap
--------------------------------------------------------------------------------

local function generate_documentation(type)
    return function ()
        require('neogen').generate({
            type = type,
            annotation_convention = _G.neogen.languages,
        })
    end
end

function M.keymap()
    map_group('n', '<leader>d', 'document')
    map('n', '<leader>dc', 'Document Class', generate_documentation('class'))
    map('n', '<leader>df', 'Document Function', generate_documentation('func'))
    map('n', '<leader>dt', 'Document Type', generate_documentation('type'))
    map('n', '<leader>dm', 'Document File/Module', generate_documentation('file'))
end

return M
