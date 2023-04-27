--------------------------------------------------------------------------------
-- LSP Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local window = require('sanka047.utils.window')
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

local M = {}

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
        local buf_map = function (...) require('sanka047.utils.map').buf_map(bufnr, ...) end

        buf_map('n', 'K', 'Doc', vim.lsp.buf.hover)

        -- illuminate
        local has_illuminate, illuminate = pcall(require, 'illuminate')
        if has_illuminate then
            illuminate.on_attach(client)
        end
    end
end

local function get_lsp_settings(server)
    return require('sanka047.plugins.lsp.servers.' .. server)
end

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
        lua_ls = get_lsp_settings('lua_ls'),
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
    ----------------------------------------------------------------------------
    -- LSP Handler Overrides
    ----------------------------------------------------------------------------
    vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(
        vim.lsp.handlers.hover,
        { border = window.border(window.margin.FULL) }
    )

    -- setup signs/highlights
    local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
    for type, icon in pairs(signs) do
        local hl = "DiagnosticSign" .. type
        vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
    end

    configure_lsp_servers()
end

--------------------------------------------------------------------------------
-- LSP Keymaps
--------------------------------------------------------------------------------
function M.keymap()
    map_group('n', '<leader>g', 'lsp-get')

    -- refactoring
    map('n', '<leader>rn', 'Rename', vim.lsp.buf.rename)

    -- get code information
    map('n', '<leader>gd', 'Get Def.', function ()
        require("telescope.builtin").lsp_definitions()
    end)
    map('n', '<leader>gD', 'Get Decl.', vim.lsp.buf.declaration)
    map('n', '<leader>gy', 'Get Type Def.', function ()
        require("telescope.builtin").lsp_type_definitions()
    end)
    map('n', '<leader>gi', 'Get Impl.', function ()
        require("telescope.builtin").lsp_implementations()
    end)
    map('n', '<leader>gr', 'Get Ref.', function ()
        require("telescope.builtin").lsp_references()
    end)

    map_group('n', '<leader>l', 'lsp')

    -- diagnostic information
    map('n', '<leader>ld', 'Find Diagnostics', function ()
        require("telescope.builtin").diagnostics()
    end)
    map('n', '[d', 'Prev Diagnostic', vim.diagnostic.goto_prev)
    map('n', ']d', 'Next Diagnostic', vim.diagnostic.goto_next)
    map('n', '[e', 'Prev Error', function ()
        vim.diagnostic.goto_prev({ severity = { min = vim.diagnostic.severity.WARN } })
    end)
    map('n', ']e', 'Next Error', function ()
        vim.diagnostic.goto_next({ severity = { min = vim.diagnostic.severity.WARN } })
    end)
    map('n', '<leader>q', 'Diagnostic (Loc)List', vim.diagnostic.setloclist)

    -- workspace operations
    map_group('n', '<leader>w', 'workspace')
    map('n', '<leader>wa', 'Add Folder', vim.lsp.buf.add_workspace_folder)
    map('n', '<leader>wr', 'Remove Folder', vim.lsp.buf.remove_workspace_folder)
    map('n', '<leader>wl', 'List Folders', function () P(vim.lsp.buf.list_workspace_folders()) end)

    map('n', '<leader>lf', 'Format', vim.lsp.buf.format)

    map('n', '<leader>lca', 'Code Actions', function ()
        require("telescope.builtin").lsp_code_actions()
    end)
    map('v', '<leader>lca', 'Code Actions', function ()
        require("telescope.builtin").lsp_range_code_actions()
    end)
end

return M
