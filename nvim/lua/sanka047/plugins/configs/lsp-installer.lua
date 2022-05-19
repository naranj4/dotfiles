--------------------------------------------------------------------------------
-- LSP Installer Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, lsp_installer = pcall(require, 'nvim-lsp-installer')
    if not ok then
        log.error('nvim-lsp-installer not available', 'Config')
        return false
    end

    lsp_installer.settings({
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
    vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(
        vim.lsp.handlers.signature_help,
        {
            border = 'rounded',
            close_events = { 'CursorMoved', 'BufHidden', 'InsertCharPre' },
        }
    )
    vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(
        vim.lsp.handlers.hover,
        { border = 'rounded' }
    )

    --------------------------------------------------------------------------------
    -- LSP Config
    --------------------------------------------------------------------------------
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
        local map = function (...) require('sanka047.utils.map').buf_map(bufnr, ...) end

        map('n', 'K', 'Doc', vim.lsp.buf.hover)

        -- illuminate
        local has_illuminate, illuminate = pcall(require, 'illuminate')
        if has_illuminate then
            illuminate.on_attach(client)
        end
    end

    -- LSP installation and config
    local servers = {
        'pyright', -- pyright
        'solargraph', -- ruby
        'yamlls', -- yaml
        'sumneko_lua', -- lua
        'tsserver', -- typescript
    }
    lsp_installer.on_server_ready(function (server)
        local opts = {
            on_attach = on_attach,
            capabilities = capabilities,
            flags = {
                debounce_text_changes = 150,
            },
        }

        -- sumneko_lua specific config
        if server.name == 'sumneko_lua' then
            local runtime_path = vim.split(package.path, ';')
            table.insert(runtime_path, 'lua/?.lua')
            table.insert(runtime_path, 'lua/?/init.lua')

            opts.settings = {
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
            }
        end

        server:setup(opts)
    end)
end

return M
