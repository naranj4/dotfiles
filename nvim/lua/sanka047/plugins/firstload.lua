--------------------------------------------------------------------------------
-- First Load Actions
--------------------------------------------------------------------------------
local function download_packer()
    if vim.fn.input('Download Packer? (y for yes)') ~= 'y' then
        return
    end

    local packer_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/'
    vim.fn.delete(packer_path, 'rf')
    vim.fn.mkdir(packer_path, 'p')

    local out = vim.fn.system({
        "git",
        "clone",
        "https://github.com/wbthomason/packer.nvim",
        "--depth",
        "20",
        packer_path .. '/packer.nvim',
    })

    print(out)
    print('Downloading packer.nvim...')
    print("( You'll need to restart now )")
end

if not pcall(require, 'packer') then
    download_packer()
    return true
end
return false
