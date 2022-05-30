--------------------------------------------------------------------------------
-- Window Functions
--------------------------------------------------------------------------------
local M = {}

function M.win_move(direction)
    local curwin = vim.api.nvim_win_get_number(0)
    vim.cmd('wincmd ' .. direction)
    if curwin == vim.api.nvim_win_get_number(0) then
        if direction == 'j' or direction == 'k' then
            vim.cmd('wincmd s')
        else
            vim.cmd('wincmd v')
        end
        vim.cmd('wincmd ' .. direction)
    end
end

--------------------------------------------------------------------------------
-- Quickfix Functions
--------------------------------------------------------------------------------
M.is_qf_list_open = false
M.is_loc_list_open = false

function M.toggle_qf_list(is_qf)
    if is_qf then
        if vim.bo.filetype == 'qf' or M.is_qf_list_open then
            vim.cmd('cclose')
            M.is_qf_list_open = false
        else
            vim.cmd('botright copen')
            M.is_qf_list_open = true
        end
    else
        if vim.bo.filetype == 'qf' or M.is_loc_list_open then
            vim.cmd('lclose')
            M.is_qf_list_open = false
        else
            vim.cmd('lopen')
            M.is_qf_list_open = true
        end
    end
end

function M.set_qf_control_var()
    vim.cmd([[
        if getwininfo(win_getid())[0]['loclist'] == 1
            lua require('sanka047.utils.window').is_loc_list_open = true
        else
            lua require('sanka047.utils.window').is_qf_list_open = true
        end
    ]])
end

function M.unset_qf_control_var()
    vim.cmd([[
        if getwininfo(win_getid())[0]['loclist'] == 1
            lua require('sanka047.utils.window').is_loc_list_open = false
        else
            lua require('sanka047.utils.window').is_qf_list_open = false
        end
    ]])
end

return M
