--------------------------------------------------------------------------------
-- LSP Installer Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

--------------------------------------------------------------------------------
-- LSP Config
--------------------------------------------------------------------------------
local function get_client_capabilities()
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

    return capabilities
end

local function generate_on_attach()
    return function (client, bufnr)
        local map = function (...) require('sanka047.utils.map').buf_map(bufnr, ...) end

        map('n', 'K', 'Doc', vim.lsp.buf.hover)

        -- illuminate
        local has_illuminate, illuminate = pcall(require, 'illuminate')
        if has_illuminate then
            illuminate.on_attach(client)
        end

        local has_aerial, aerial = pcall(require, 'aerial')
        if has_aerial then
            aerial.on_attach(client, bufnr)
        end
    end
end

local function get_lsp_settings(server) return require('sanka047.lsp.' .. server) end

local function configure_lsp_servers()
    local has_lspconfig, lspconfig = pcall(require, 'lspconfig')
    if not has_lspconfig then
        log.error('lspconfig not available', 'Config')
        return false
    end

    local capabilities = get_client_capabilities()
    local on_attach = generate_on_attach()

    -- LSP installation and config
    local servers = {
        pyright = {}, -- pyright
        solargraph = {}, -- ruby
        yamlls = {}, -- yaml
        tsserver = {}, -- typescript
        sumneko_lua = get_lsp_settings('sumneko_lua'),
    }

    -- setup all language servers with default configuration
    for server, server_opts in pairs(servers) do
        local opts = vim.tbl_deep_extend('force', {
            on_attach = on_attach,
            capabilities = capabilities,
        }, server_opts)

        lspconfig[server].setup(opts)
    end
end

function M.setup()
    local has_lsp_installer, lsp_installer = pcall(require, 'nvim-lsp-installer')
    if not has_lsp_installer then
        log.error('nvim-lsp-installer not available', 'Config')
        return false
    end

    lsp_installer.setup({
        ui = {
            icons = {
                server_installed = "✓",
                server_pending = "➜",
                server_uninstalled = "✗",
            },
        },
    })

    --------------------------------------------------------------------------------
    -- LSP Handlers
    --------------------------------------------------------------------------------
    vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(
        vim.lsp.handlers.hover,
        { border = 'rounded' }
    )

    configure_lsp_servers()
end

return M
