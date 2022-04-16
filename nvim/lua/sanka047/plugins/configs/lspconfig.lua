--------------------------------------------------------------------------------
-- LSP Config
--------------------------------------------------------------------------------
local ok, lspconfig = pcall(require, 'lspconfig')
if not ok then
    vim.notify('lspconfig not available', 'error')
    return false
end

-- local ok, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
-- if not ok then
--     vim.notify('cmp_nvim_lsp not available', 'error')
--     return false
-- end

local capabilities = vim.lsp.protocol.make_client_capabilities()
-- capabilities = cmp_nvim_lsp.update_capabilities(capabilities)
capabilities.textDocument.completion.completionItem.documentationFormat = { "markdown", "plaintext" }
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.preselectSupport = true
capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
capabilities.textDocument.completion.completionItem.deprecatedSupport = true
capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
capabilities.textDocument.completion.completionItem.tagSupport = { valueSet = { 1 } }
capabilities.textDocument.completion.completionItem.resolveSupport = {
    properties = {
        "documentation",
        "detail",
        "additionalTextEdits",
    },
}

local function on_attach(client, bufnr)
    local map = require('sanka047.core.utils').map
    local map_group = require('sanka047.core.utils').map_group

    map('n', 'K', 'Doc', '<CMD>lua vim.lsp.buf.hover()<CR>')
    map('n', '<leader>rn', 'Rename', '<CMD>lua vim.lsp.buf.rename()<CR>')

    -- diagnostic information
    map('n', '[d', 'Prev Diagnostic', '<CMD>lua vim.diagnostic.goto_prev()<CR>')
    map('n', ']d', 'Next Diagnostic', '<CMD>lua vim.diagnostic.goto_next()<CR>')
    map('n', '<leader>q', 'Diagnostic (Loc)List', '<CMD>lua vim.diagnostic.setloclist()<CR>')

    -- workspace operations
    map_group('n', '<leader>w', 'workspace')
    map('n', '<leader>wa', 'Add Folder', '<CMD>lua vim.lsp.buf.add_workspace_folder()<CR>')
    map('n', '<leader>wr', 'Remove Folder', '<CMD>lua vim.lsp.buf.remove_workspace_folder()<CR>')
    map('n', '<leader>wl', 'List Folders', '<CMD>lua P(vim.lsp.buf.list_workspace_folders())<CR>')

    -- illuminate
    local has_illuminate, illuminate = pcall(require, 'illuminate')
    if has_illuminate then
        illuminate.on_attach(client)
    end
end

local servers = {'pyright', 'solargraph', 'yamlls'}
for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup({
        on_attach = on_attach,
        capabilities = capabilities,
        flags = {
            debounce_text_changes = 150,
        },
    })
end

-- Lua LSP
local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

lspconfig.sumneko_lua.setup({
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
        Lua = {
            runtime = {
                version = 'LuaJIT',
                path = runtime_path,
            },
            diagnostics = {
                globals = {'vim'},
            },
            workspace = {
                library = vim.api.nvim_get_runtime_file('', true),
            },
            telemetry = {
                enable = false,
            },
        },
    },
})
