--------------------------------------------------------------------------------
-- LSP, Snippets and Autocomplete Config
--------------------------------------------------------------------------------
local common = require('common')
local map = common.map
local lspconfig = require('lspconfig')
local luasnip = require('luasnip')
local cmp = require('cmp')

--------------------------------------------------------------------------------
-- Nvim-Cmp and LuaSnip
--------------------------------------------------------------------------------
vim.o.completeopt = 'menu,menuone,noselect'

cmp.setup({
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end,
    },
    mapping = {
        ['<C-p>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 'c' }),
        ['<C-n>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 'c' }),
        ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
        ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
        ['<C-e>'] = cmp.mapping(cmp.mapping.close(), { 'i', 'c' }),
        ['<CR>'] = cmp.mapping(cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
        }), { 'i' }),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
        ['<Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then luasnip.expand_or_jump()
            else fallback()
            end
        end, { 'i', 'c' }),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then luasnip.jump(-1)
            else fallback()
            end
        end, { 'i', 'c' }),
    },
    sources = {
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
        { name = 'buffer' },
        { name = 'path' },
        { name = 'cmdline' },
    }
})

--------------------------------------------------------------------------------
-- Nvim-Cmp Sources
--------------------------------------------------------------------------------
cmp.setup.cmdline(':', {
    sources = cmp.config.sources(
        { { name = 'cmdline' }, },
        { { name = 'path' }, }
    ),
})

cmp.setup.cmdline('/', {
    sources = {
        { name = 'buffer' },
    },
})

--------------------------------------------------------------------------------
-- LSPConfig
--------------------------------------------------------------------------------
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
local function on_attach(client, bufnr)
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    map('n', 'K', '<CMD>lua vim.lsp.buf.hover()<CR>')
    map('n', '<leader>rn', '<CMD>lua vim.lsp.buf.rename()<CR>')

    -- Diagnostic information
    map('n', '[d', '<CMD>lua vim.diagnostic.goto_prev()<CR>')
    map('n', ']d', '<CMD>lua vim.diagnostic.goto_next()<CR>')
    map('n', '<leader>q', '<CMD>lua vim.diagnostic.setloclist()<CR>')

    -- Workspace operations
    map('n', '<leader>wa', '<CMD>lua vim.lsp.buf.add_workspace_folder()<CR>')
    map('n', '<leader>wr', '<CMD>lua vim.lsp.buf.remove_workspace_folder()<CR>')
    map('n', '<leader>wl', '<CMD>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>')
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
