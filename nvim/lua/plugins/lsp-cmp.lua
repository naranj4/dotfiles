--------------------------------------------------------------------------------
-- LSP, Snippets and Autocomplete Config
--------------------------------------------------------------------------------
local wk = require('which-key')
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
        { name = 'nvim_lua' },
    }
})

wk.register({
    ['<C-p>'] = '[CMP] Prev Selection',
    ['<C-n>'] = '[CMP] Next Selection',
    ['<C-d>'] = '[CMP] Scroll Docs Down',
    ['<C-f>'] = '[CMP] Scroll Docs Up',
    ['<C-e>'] = '[CMP] Close Completion Menu',
    ['<CR>'] = '[CMP] Confirm Completion',
    ['<C-Space>'] = '[CMP] Trigger Manual Completion',
    ['<Tab>'] = '[CMP] Next Item/Snippet Hook',
    ['<S-Tab>'] = '[CMP] Prev Item/Snippet Hook',
}, { mode ='i' })
wk.register({
    ['<C-p>'] = '[CMP] Prev Selection',
    ['<C-n>'] = '[CMP] Next Selection',
    ['<C-d>'] = '[CMP] Scroll Docs Down',
    ['<C-f>'] = '[CMP] Scroll Docs Up',
    ['<C-e>'] = '[CMP] Close Completion Menu',
    ['<CR>'] = '[CMP] Confirm Completion',
    ['<C-Space>'] = '[CMP] Trigger Manual Completion',
    ['<Tab>'] = '[CMP] Next Item/Snippet Hook',
    ['<S-Tab>'] = '[CMP] Prev Item/Snippet Hook',
}, { mode ='c' })

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

    wk.register({
        K = { '<CMD>lua vim.lsp.buf.hover()<CR>', 'Doc' },
        ['<leader>rn'] = { '<CMD>lua vim.lsp.buf.rename()<CR>', 'Rename' },

        -- diagnostic information
        ['[d'] = { '<CMD>lua vim.diagnostic.goto_prev()<CR>', 'Prev Diagnostic' },
        [']d'] = { '<CMD>lua vim.diagnostic.goto_next()<CR>', 'Next Diagnostic' },
        ['<leader>q'] = { '<CMD>lua vim.diagnostic.setloclist()<CR>', 'Diagnostic (Loc)List' },

        -- workspace operations
        ['<leader>wa'] = { '<CMD>lua vim.lsp.buf.add_workspace_folder()<CR>', 'Rename' },
        ['<leader>wr'] = { '<CMD>lua vim.lsp.buf.remove_workspace_folder()<CR>', 'Rename' },
        ['<leader>wl'] = { '<CMD>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', 'Rename' },
    }, { mode = 'n' })
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

require('lspconfig').sumneko_lua.setup({
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

--------------------------------------------------------------------------------
-- Nvim-Autopairs Compatibility
--------------------------------------------------------------------------------
-- If you want insert `(` after select function or method item
local cmp_autopairs = require('nvim-autopairs.completion.cmp')
cmp.event:on( 'confirm_done', cmp_autopairs.on_confirm_done({ map_char = { tex = '' } }))

-- add a lisp filetype (wrap my-function), FYI: Hardcoded = { "clojure", "clojurescript", "fennel", "janet" }
cmp_autopairs.lisp[#cmp_autopairs.lisp+1] = "racket"
