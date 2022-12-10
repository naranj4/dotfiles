--------------------------------------------------------------------------------
-- Nvim-Cmp Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local window = require('sanka047.utils.window')
local map = require('sanka047.utils.map').map

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

    ----------------------------------------------------------------------------
    -- Nvim-Cmp Setup
    ----------------------------------------------------------------------------
    local has_lspkind, lspkind = pcall(require, 'lspkind')
    if not has_lspkind then
        log.error('lspkind not available (cmp)', 'Config')
        return false
    end

    local select_item_down = function (fallback)
        if cmp.visible() then
            if cmp.core.view.custom_entries_view:is_direction_top_down() then
                cmp.select_next_item()
            else
                cmp.select_prev_item()
            end
        else
            fallback()
        end
    end

    local select_item_up = function (fallback)
        if cmp.visible() then
            if cmp.core.view.custom_entries_view:is_direction_top_down() then
                cmp.select_prev_item()
            else
                cmp.select_next_item()
            end
        else
            fallback()
        end
    end

    cmp.setup({
        snippet = {
            expand = function(args)
                luasnip.lsp_expand(args.body)
            end,
        },
        completion = { completeopt = 'menu,menuone,noinsert' },
        formatting = {
            fields = { 'kind', 'abbr', 'menu' },
            format = function (entry, vim_item)
                local kind = lspkind.cmp_format({
                    mode = 'symbol_text',
                    menu = {
                        buffer = '[Buf]',
                        nvim_lsp = '[LSP]',
                        luasnip = '[Snp]',
                        nvim_lua = '[Lua]',
                        latex_symbols = '[Ltx]',
                    },
                    maxwidth = 50,
                })(entry, vim_item)

                local strings = vim.split(kind.kind, '%s', { trimempty = true })
                kind.kind = ' ' .. strings[1] .. ' '
                kind.menu = '    ' .. (kind.menu or '     ') .. ' (' .. strings[2] .. ')'

                return kind
            end
        },
        view = {
            entries = { name = 'custom', selection_order = 'near_cursor' },
        },
        window = {
            completion = cmp.config.window.bordered({
                border = window.border(window.margin.NONE),
                winhighlight = 'FloatBorder:CmpCompletionBorder,NormalFloat:CmpCompletion,Search:None',
                col_offset = -3,
                side_padding = 0,
            }),
            documentation = cmp.config.window.bordered({
                border = window.border(window.margin.FULL),
                winhighlight = 'FloatBorder:CmpDocumentationBorder,NormalFloat:CmpDocumentation,Search:None',
            }),
        },
        mapping = {
            ['<Up>'] = cmp.mapping(select_item_up, { 'i' }),
            ['<Down>'] = cmp.mapping(select_item_down, { 'i' }),
            ['<C-p>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 'c' }),
            ['<C-n>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 'c' }),

            ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
            ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
            ['<C-g>'] = cmp.mapping(cmp.mapping.close(), { 'i', 'c' }),

            ['<Tab>'] = cmp.mapping(cmp.mapping.confirm({
                behavior = cmp.ConfirmBehavior.Replace,
                select = true,
            }), { 'i', 'c' }),
            ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
            ['<C-e>'] = cmp.mapping(cmp.mapping.abort(), { 'i', 'c' }),
        },
        sources = cmp.config.sources({
            { name = 'nvim_lsp' },
            { name = 'luasnip' },
            { name = 'path' },
        }, {
            { name = 'buffer' },
        }),
    })

    ----------------------------------------------------------------------------
    -- Nvim-Cmp Sources
    ----------------------------------------------------------------------------
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

    ----------------------------------------------------------------------------
    -- Nvim-Autopairs Compatibility
    ----------------------------------------------------------------------------
    local has_autopairs, cmp_autopairs = pcall(require, 'nvim-autopairs.completion.cmp')
    if has_autopairs then
        cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done({ map_char = { tex = '' } }))
    end
end

--------------------------------------------------------------------------------
-- Nvim-Cmp Keymap
--------------------------------------------------------------------------------
function M.keymap()
    map('ic', '<C-p>', 'Select Prev')
    map('ic', '<C-n>', 'Select Next')
    map('ic', '<S-Tab>', 'Select Prev')
    map('ic', '<Tab>', 'Select Next')

    map('ic', '<C-d>', 'Scroll Down (Docs)')
    map('ic', '<C-f>', 'Scroll Up (Docs)')

    map('ic', '<C-e>', 'Close')
    map('ic', '<CR>', 'Confirm')
    map('ic', '<C-Space>', 'Trigger Completion')

    map('ic', '<C-j>', 'Snippet Hop Forward')
    map('ic', '<C-k>', 'Snippet Hop Backward')
end

return M
