--------------------------------------------------------------------------------
-- Plugins
--------------------------------------------------------------------------------
local exec = vim.api.nvim_exec      -- execute vimscript
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

-- auto compile plugins on write
exec([[
    augroup packer_user_config
        autocmd!
        autocmd BufWritePost plug.lua source <afile> | PackerCompile
    augroup end
]], false)

return require('packer').startup(function(use)
    -- let packer manager itself
    use {'wbthomason/packer.nvim'}

    -- colorschemes
    use {'whatyouhide/vim-gotham'}  -- not treesitter compatible, but awesome
    use {'folke/tokyonight.nvim'}
    use {'marko-cerovac/material.nvim'}  -- pretty nice, a tad bright
    use {'Yagua/nebulous.nvim'}

    use {
        'nvim-lualine/lualine.nvim',
        requires = {{'kyazdani42/nvim-web-devicons'}}
    }

    use {
        'kyazdani42/nvim-tree.lua',
        requires = {{'kyazdani42/nvim-web-devicons'}}
    }
    use {
        'lewis6991/gitsigns.nvim',
        requires = {
            {'nvim-lua/plenary.nvim'},
        }
    }

    use {'machakann/vim-sandwich'}
    use {'windwp/nvim-autopairs'}

    use {'terrortylor/nvim-comment'}

    use {'phaazon/hop.nvim', branch = 'v1'}

    use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
    use {
        'nvim-telescope/telescope.nvim',
        requires = {
            {'nvim-lua/plenary.nvim'},
            {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'},
            {'kyazdani42/nvim-web-devicons'},
        }
    }

    use {'neovim/nvim-lspconfig'}

    -- Autocompletion (coq_nvim)
    use {
        'ms-jpq/coq_nvim',
        branch = 'coq',
        requires = {
            {'ms-jpq/coq.artifacts', branch = 'artifacts'},  -- snippets
            {'ms-jpq/coq.thirdparty', branch = '3p'},
        },
    }

    -- Autocompletion (nvim-cmp)
    -- use {'hrsh7th/cmp-nvim-lsp'}
    -- use {'hrsh7th/cmp-buffer'}
    -- use {'hrsh7th/cmp-path'}
    -- use {'hrsh7th/cmp-cmdline'}
    -- use {'hrsh7th/nvim-cmp'}

    -- Snippets
    -- use {'L3MON4D3/LuaSnip'}
    -- use {'saadparwaiz1/cmp_luasnip'}

    if packer_bootstrap then
        require('packer').sync()
    end
end)
