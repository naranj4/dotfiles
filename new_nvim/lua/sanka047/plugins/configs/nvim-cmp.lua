--------------------------------------------------------------------------------
-- Nvim-Cmp Config
--------------------------------------------------------------------------------
-- TODO: Shit is broken
local has_cmp, cmp = pcall(require, 'cmp')
if not has_cmp then
    print('nvim-cmp not available')
    return false
end

local has_luasnip, luasnip = pcall(require, 'luasnip')
if not has_luasnip then
    print('luasnip not available (cmp)')
    return false
end

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
        ['<S-Tab>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 'c' }),
        ['<Tab>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 'c' }),

        ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
        ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
        ['<C-e>'] = cmp.mapping(cmp.mapping.close(), { 'i', 'c' }),
        ['<CR>'] = cmp.mapping(cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = false, -- false will not auto-select?
        }), { 'i', 'c' }),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),

        ['<C-j>'] = cmp.mapping(function (fallback)
            luasnip.expand_or_jump()
        end, { 'i', 'c' }),
        ['<C-k>'] = cmp.mapping(function (fallback)
            luasnip.jump(-1)
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
-- Nvim-Autopairs Compatibility
--------------------------------------------------------------------------------
local has_autopairs, cmp_autopairs = pcall(require, 'nvim-autopairs.completion.cmp')
if has_autopairs then
    cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done({ map_char = { tex = '' } }))

    -- add a lisp filetype (wrap my-function), FYI: Hardcoded = { "clojure", "clojurescript", "fennel", "janet" }
    cmp_autopairs.lisp[#cmp_autopairs.lisp+1] = "racket"
end
