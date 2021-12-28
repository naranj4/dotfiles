--------------------------------------------------------------------------------
-- LSP and Autocomplete Config
--------------------------------------------------------------------------------
local common = require('common')
local map = common.map
local lspconfig = require('lspconfig')

--------------------------------------------------------------------------------
-- Coq_nvim
--------------------------------------------------------------------------------
vim.g.coq_settings = {
    auto_start = true,
    keymap = {
        recommended = false,
        pre_select = false,

        manual_complete = '<c-space>',
        bigger_preview = '<c-k>',
        jump_to_mark = '<c-m>',
        eval_snips = '<leader>se'
    },
    clients = {
        lsp = {
            enabled = true,
            resolve_timeout = 0.06,
        },
        snippets = {
            enabled = true,
        },
        paths = { enabled = true },
        tree_sitter = {
            enabled = true,
            search_context = 333,
            slow_threshold = 0.1,
        },
        buffers = { enabled = true },
        tmux = { enabled = true },
        third_party = {
            enabled = true,
        },
    },
    display = {
        pum = { fast_close = false },
        preview = {
            border = 'rounded',
            positions = {
                ['north'] = 1,
                ['south'] = 2,
                ['west'] = 3,
                ['east'] = 4,
            },
        },
    },
}

--------------------------------------------------------------------------------
-- LSPConfig
--------------------------------------------------------------------------------
local lspconfig = require('lspconfig')
local coq = require('coq')

local capabilities = vim.lsp.protocol.make_client_capabilities()
local function on_attach(client, bufnr)
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    map('n', 'K', '<CMD>lua vim.lsp.buf.hover()<CR>')
    map('n', '<leader>rn', '<CMD>lua vim.lsp.buf.rename()<CR>')
    map('n', '[d', '<CMD>lua vim.lsp.diagnostic.goto_prev()<CR>')
    map('n', ']d', '<CMD>lua vim.lsp.diagnostic.goto_next()<CR>')

    map('i', '<esc>', [[pumvisible() ? "<c-e><esc>" : "<esc>"]], { expr = true })
    map('i', '<c-c>', [[pumvisible() ? "<c-e><c-c>" : "<c-c>"]], { expr = true })
    map('i', '<tab>', [[pumvisible() ? "<c-n>" : "<tab>"]], { expr = true })
    map('i', '<s-tab>', [[pumvisible() ? "<c-p>" : "<bs>"]], { expr = true })
end

local servers = {'pyright', 'solargraph', 'yamlls'}
for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup(coq.lsp_ensure_capabilities({
        on_attach = on_attach,
        capabilities = capabilities,
        flags = {
            debounce_text_changes = 150,
        },
    }))
end

--------------------------------------------------------------------------------
-- 3rd Party Integration
--------------------------------------------------------------------------------
require('coq_3p') {
    {
        src = 'nvimlua',
        short_name = 'nLUA',
        conf_only = true,
    },
    {
        src = 'repl',
        sh = 'zsh',
        max_lines = 99,
        deadline = 500,
        unsafe = {'rm', 'poweroff', 'mv', 'sudo', 'touch', 'cp'},
    },
}
