--------------------------------------------------------------------------------
-- Nvim-Cmp Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local has_cmp, cmp = pcall(require, 'cmp')
    if not has_cmp then
        log.error('nvim-cmp not available', 'Config')
        return false
    end

    local has_luasnip, luasnip = pcall(require, 'luasnip')
    if not has_luasnip then
        log.error('luasnip not available (cmp)', 'Config')
        return false
    end

    vim.opt.completeopt = 'menu,menuone,noselect'

    --------------------------------------------------------------------------------
    -- Nvim-Cmp Setup
    --------------------------------------------------------------------------------
    local has_lspkind, lspkind = pcall(require, 'lspkind')
    if not has_lspkind then
        log.error('lspkind not available (cmp)', 'Config')
        return false
    end

    cmp.setup({
        snippet = {
            expand = function(args)
                luasnip.lsp_expand(args.body)
            end,
        },
        formatting = {
            format = lspkind.cmp_format({
                mode = 'symbol_text',
                menu = ({
                    buffer = "[Buf]",
                    nvim_lsp = "[LSP]",
                    luasnip = "[Snip]",
                    nvim_lua = "[Lua]",
                    latex_symbols = "[Latex]",
                }),
                maxwidth = 50,
                -- The function below will be called before any actual modifications from lspkind
                -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
                before = function (entry, vim_item)
                    return vim_item
                end,
            })
        },
        view = {
            entries = { name = 'custom', selection_order = 'near_cursor' },
        },
        window = {
            completion = cmp.config.window.bordered(
                {
                    border = {'', '', '', ' ', '', '', '', ' '},
                    winhighlight = 'FloatBorder:CmpCompletion,NormalFloat:CmpCompletion',
                }
            ),
            documentation = cmp.config.window.bordered(),
        },
        mapping = {
            ['<S-Tab>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 'c' }),
            ['<Tab>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 'c' }),

            ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
            ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
            ['<C-e>'] = cmp.mapping(cmp.mapping.close(), { 'i', 'c' }),

            ['<CR>'] = cmp.mapping(cmp.mapping.confirm({
                behavior = cmp.ConfirmBehavior.Replace,
                select = false, -- false will not auto-select?
            }), { 'i', 'c' }),
            ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
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
end

return M
