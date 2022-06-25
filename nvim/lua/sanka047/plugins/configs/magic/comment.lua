--------------------------------------------------------------------------------
-- Comment Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map

local M = {}

function M.setup()
    local ok, Comment = pcall(require, 'Comment')
    if not ok then
        log.error('Comment not available', 'Config')
        return false
    end

    Comment.setup({
        ---Add a space b/w comment and the line
        ---@type boolean
        padding = true,

        ---Whether the cursor should stay at its position
        ---NOTE: This only affects NORMAL mode mappings and doesn't work with dot-repeat
        ---@type boolean
        sticky = true,

        ---Lines to be ignored while comment/uncomment.
        ---Could be a regex string or a function that returns a regex string.
        ---Example: Use '^$' to ignore empty lines
        ---@type string|fun():string
        ignore = '^$',

        ---LHS of toggle mappings in NORMAL + VISUAL mode
        ---@type table
        toggler = {
            ---Line-comment toggle keymap
            line = '<leader>cc',
            ---Block-comment toggle keymap
            block = '<leader>bc',
        },

        ---LHS of operator-pending mappings in NORMAL + VISUAL mode
        ---@type table
        opleader = {
            ---Line-comment keymap
            line = '<leader>c',
            ---Block-comment keymap
            block = '<leader>b',
        },

        ---LHS of extra mappings
        ---@type table
        extra = {
            ---Add comment on the line above
            above = '<leader>cO',
            ---Add comment on the line below
            below = '<leader>co',
            ---Add comment at the end of line
            eol = '<leader>cA',
        },

        ---Create basic (operator-pending) and extended mappings for NORMAL + VISUAL mode
        ---@type table
        mappings = {
            ---Operator-pending mapping
            ---Includes `gcc`, `gbc`, `gc[count]{motion}` and `gb[count]{motion}`
            ---NOTE: These mappings can be changed individually by `opleader` and `toggler` config
            basic = false,
            ---Extra mapping
            ---Includes `gco`, `gcO`, `gcA`
            extra = false,
            ---Extended mapping
            ---Includes `g>`, `g<`, `g>[count]{motion}` and `g<[count]{motion}`
            extended = false,
        },

        ---Pre-hook, called before commenting the line
        pre_hook = function (ctx)
            local utils = require('Comment.utils')

            local has_ctx_utils, ts_ctx_utils = pcall(require, 'ts_context_commentstring.utils')
            if not has_ctx_utils then
                require('sanka047.utils.log').warn(
                    'ts_context_commentstring is unavailable to provide commentstring',
                    'Config [Comment]'
                )
                return
            end

            -- Determine whether to use linewise or blockwise commentstring
            local type = ctx.ctype == utils.ctype.line and '__default' or '__multiline'

            -- Determine the location where to calculate commentstring from
            local location = nil
            if ctx.ctype == utils.ctype.block then
                location = ts_ctx_utils.get_cursor_location()
            elseif ctx.cmotion == utils.cmotion.v or ctx.cmotion == utils.cmotion.V then
                location = ts_ctx_utils.get_visual_start_location()
            end

            return require('ts_context_commentstring.internal').calculate_commentstring({
                key = type,
                location = location,
            })
        end,

        ---Post-hook, called after commenting is done
        post_hook = nil,
    })
end

--------------------------------------------------------------------------------
-- Comment Keymap
--------------------------------------------------------------------------------
function M.keymap()
    -- Line comments
    map(
        'n',
        '<leader>c',
        'Line Comment',
        '<CMD>lua require("Comment.api").call("toggle_linewise_op")<CR>g@'
    )
    map(
        'x',
        '<leader>c',
        'Line Comment',
        '<ESC><CMD>lua require("Comment.api").toggle_linewise_op(vim.fn.visualmode())<CR>'
    )

    map(
        'n',
        '<leader>cc',
        'Toggle Line Comment',
        '<CMD>lua require("Comment.api").toggle_current_linewise()<CR>'
    )

    -- Block comments
    map(
        'n',
        '<leader>b',
        'Block Comment',
        '<CMD>lua require("Comment.api").call("toggle_blockwise_op")<CR>g@'
    )
    map(
        'x',
        '<leader>b',
        'Block Comment',
        '<ESC><CMD>lua require("Comment.api").toggle_blockwise_op(vim.fn.visualmode())<CR>'
    )
    map(
        'n',
        '<leader>bc',
        'Toggle Block Comment',
        '<CMD>lua require("Comment.api").toggle_current_blockwise()<CR>'
    )

    -- Extra mappings
    map(
        'n',
        '<leader>co',
        'Add Comment Above',
        '<CMD>lua require("Comment.api").insert_linewise_below()<CR>'
    )
    map(
        'n',
        '<leader>cO',
        'Add Comment Below',
        '<CMD>lua require("Comment.api").insert_linewise_above()<CR>'
    )
    map(
        'n',
        '<leader>cA',
        'Add Comment (EOL)',
        '<CMD>lua require("Comment.api").insert_linewise_eol()<CR>'
    )
end

return M
