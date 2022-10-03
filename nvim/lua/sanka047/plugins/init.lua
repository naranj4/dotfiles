--------------------------------------------------------------------------------
-- Plugins
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local registry = require('sanka047.registry')
local firstload = require('sanka047.plugins.firstload')

if firstload then
    require('sanka047.utils.map').create_autocmd('User', 'Notify user to reload after bootstrap', {
        pattern = 'PackerComplete',
        callback = function ()
            print('-------------------------------------------')
            print('Packer has finished bootstrapping.')
            print('Plugins have been installed.')
            print('Please restart neovim.')
            print('-------------------------------------------')
        end,
        once = true,
    })
end

local packer = require('packer')
local window = require('sanka047.utils.window')

packer.init({
    display = {
        open_fn = function()
            return require('packer.util').float(
                { border = window.border(window.margin.FULL) }
            )
        end,
        prompt_border = window.border(window.margin.FULL),
    },
    git = {
        clone_timeout = 300, -- seconds
    },
    auto_clean = true,
    compile_on_sync = true,
    profile = {
        enable = true,
        threshold = 1,
    },
})

--------------------------------------------------------------------------------
-- Keymaps
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

map('n', '<leader><leader>ps', 'Packer Sync', require('sanka047.utils.packer').packer_sync)
map('n', '<leader><leader>pc', 'Packer Compile', function () packer.compile() end)

--------------------------------------------------------------------------------
-- Lazy loading helpers
--------------------------------------------------------------------------------
local function _lazy_load_condition(metadata)
    log.debug('Metadata: ' .. vim.inspect(metadata))
    local ignore_filetypes = {
        TelescopePrompt = true,
        TelescopeResults = true,
        NvimTree = true,
        DressingInput = true,
        alpha = true,
    }

    local ft = vim.bo[metadata.buf].filetype
    if ignore_filetypes[ft] then
        log.debug('Not lazy loading plugins in filetype `' .. ft .. '`')
        return false
    end

    log.debug('Lazy loading plugins in filetype `' .. ft .. '`')
    return true
end

local function _lazy_load_is_complete(metadata)
    log.debug('Metadata: ' .. vim.inspect(metadata))
    if metadata.num_successes > 0 then
        log.debug('Removing lazy load command for ' .. metadata.key)
        return true
    end

    return false
end

local function _lazy_load_plugin(metadata)
    log.debug('Metadata: ' .. vim.inspect(metadata))

    local ft = vim.bo[metadata.buf].filetype
    vim.schedule(function ()
        log.debug('Lazy loading ' .. metadata.key .. ' in filetype: `' .. ft .. '`')
        packer.loader(metadata.key)
    end)
end

--------------------------------------------------------------------------------
-- Lazy loading
--------------------------------------------------------------------------------
local function lazy_load_on_event(event, plugin)
    registry.register('LazyLoad', event, plugin, {
        callback = _lazy_load_plugin,
        condition = _lazy_load_condition,
        is_complete = _lazy_load_is_complete,
    })
end

-- List of plugins lazy loaded on an event
local lazy_loaded = {
    ['nvim-autopairs'] = 'InsertEnter',
    ['nvim-cmp'] = 'BufRead',
}
for plugin, event in pairs(lazy_loaded) do
    lazy_load_on_event(event, plugin)
end

--------------------------------------------------------------------------------
-- Plugin List
--------------------------------------------------------------------------------
return require('packer').startup(function(use)
    -- let packer manager itself
    use {'wbthomason/packer.nvim'}

    ----------------------------------------------------------------------------
    -- Utility
    ----------------------------------------------------------------------------
    use {'lewis6991/impatient.nvim'}
    use {'dstein64/vim-startuptime'}
    use {'nvim-lua/plenary.nvim'}

    ----------------------------------------------------------------------------
    -- Document Mapping
    ----------------------------------------------------------------------------
    use {
        'folke/which-key.nvim',
        config = function () LOAD_CONFIG('misc.which-key') end,
    }

    ----------------------------------------------------------------------------
    -- Color Visualization
    ----------------------------------------------------------------------------
    use {
        'norcalli/nvim-colorizer.lua',
        cmd = 'ColorizerToggle',
        config = function () LOAD_CONFIG('colors.colorizer') end,
    }

    ----------------------------------------------------------------------------
    -- Colorschemes
    ----------------------------------------------------------------------------
    use {
        'tomasiser/vim-code-dark',
        disable = true,
        config = function () LOAD_CONFIG('colors.code-dark') end,
    } -- pretty nice and visible
    use {
        'rebelot/kanagawa.nvim',
        config = function () LOAD_CONFIG('colors.kanagawa') end,
    } -- nice, nice highlighting, no real complaints off the top of my head, nice completion menu
    use {'savq/melange', disable = true}

    ----------------------------------------------------------------------------
    -- Aesthetics & UI
    ----------------------------------------------------------------------------
    use {
        'kyazdani42/nvim-web-devicons',
        config = function () LOAD_CONFIG('ui.devicons') end,
    }
    use {
        'stevearc/dressing.nvim',
        config = function () LOAD_CONFIG('ui.dressing') end,
    }
    use {
        'rcarriga/nvim-notify',
        config = function () LOAD_CONFIG('ui.notify') end,
    }
    use {
        'j-hui/fidget.nvim',
        config = function () LOAD_CONFIG('ui.fidget') end,
    }

    use {
        'RRethy/vim-illuminate',
        config = function () LOAD_CONFIG('ui.illuminate') end,
    }

    use {
        'lukas-reineke/indent-blankline.nvim',
        config = function () LOAD_CONFIG('ui.indentline') end,
    }
    use {
        'nvim-treesitter/nvim-treesitter-context',
        requires = {'nvim-treesitter/nvim-treesitter'},
        config = function () LOAD_CONFIG('ui.context') end,
    }
    use {
        'nvim-lualine/lualine.nvim',
        requires = { {'kyazdani42/nvim-web-devicons'} },
        config = function () LOAD_CONFIG('ui.lualine') end,
    }
    use { -- INVESTIGATE
        'b0o/incline.nvim',
        config = function () LOAD_CONFIG('ui.incline') end,
    }

    use {
        'goolord/alpha-nvim',
        config = function () LOAD_CONFIG('ui.alpha') end,
    }

    use {
        'elihunter173/dirbuf.nvim',
        disable = true,
        config = function ()
            LOAD_CONFIG('ui.dirbuf')
            LOAD_MAPPING('ui.dirbuf')
        end
    }
    use {
        'kevinhwang91/nvim-bqf',
        requires = {
            { 'nvim-treesitter/nvim-treesitter' },
            { 'junegunn/fzf', run = function() vim.fn['fzf#install']() end },
        },
        config = function () LOAD_CONFIG('ui.bqf') end,
    }
    use {
        'nacro90/numb.nvim',
        config = function () LOAD_CONFIG('ui.numb') end,
    }
    use {
        'akinsho/toggleterm.nvim',
        config = function ()
            LOAD_CONFIG('ui.toggleterm')
            LOAD_MAPPING('ui.toggleterm')
        end,
    }

    ----------------------------------------------------------------------------
    -- Git
    ----------------------------------------------------------------------------
    use {
        'lewis6991/gitsigns.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        config = function () LOAD_CONFIG('git.gitsigns') end,
    }
    use {
        'tpope/vim-fugitive',
        config = function () LOAD_MAPPING('git.fugitive') end,
    }
    use {
        'sindrets/diffview.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        config = function ()
            LOAD_CONFIG('git.diffview')
            LOAD_MAPPING('git.diffview')
        end,
    }
    use {
        'TimUntersberger/neogit',
        requires = { {'sindrets/diffview.nvim'} },
        module = 'neogit',
        disable = true, -- still not properly using this
        config = function () LOAD_CONFIG('git.neogit') end,
    }

    ----------------------------------------------------------------------------
    -- Editing Magic
    ----------------------------------------------------------------------------
    use {
        'windwp/nvim-autopairs',
        opt = true,
        config = function ()
            LOAD_CONFIG('magic.autopairs')
            LOAD_MAPPING('magic.autopairs')
        end,
    }
    use {
        'machakann/vim-sandwich',
        config = function ()
            LOAD_CONFIG('magic.sandwich')
            LOAD_MAPPING('magic.sandwich')
        end,
    }
    use {
        'junegunn/vim-easy-align',
        config = function () LOAD_MAPPING('magic.easy-align') end,
    }
    use {
        'AckslD/nvim-trevJ.lua',
        config = function ()
            LOAD_CONFIG('magic.trevj')
            LOAD_MAPPING('magic.trevj')
        end,
    }
    use {
        'danymat/neogen',
        module = 'neogen',
        requires = 'nvim-treesitter/nvim-treesitter',
        config = function () LOAD_CONFIG('magic.neogen') end,
    }
    use {
        'numToStr/Comment.nvim',
        config = function () LOAD_CONFIG('magic.comment') end,
    }
    use {'tpope/vim-abolish'}
    use {
        'gbprod/substitute.nvim',
        module = 'substitute',
        config = function () LOAD_CONFIG('magic.substitute') end,
    }
    use {
        'ThePrimeagen/refactoring.nvim',
        module = 'sanka047.utils.refactor',
        requires = {
            {'nvim-lua/plenary.nvim'},
            {'nvim-treesitter/nvim-treesitter'},
        },
        config = function () LOAD_CONFIG('magic.refactoring') end,
    }

    ----------------------------------------------------------------------------
    -- Movement & File Navigation
    ----------------------------------------------------------------------------
    use {
        'phaazon/hop.nvim',
        branch = 'v1',
        module = 'hop',
        config = function () LOAD_CONFIG('nav.hop') end,
    }
    use {'unblevable/quick-scope'}
    use {
        'ggandor/leap.nvim',
        config = function ()
            LOAD_CONFIG('nav.leap')
            LOAD_MAPPING('nav.leap')
        end
    }
    use {
        'abecodes/tabout.nvim',
        requires = {'nvim-treesitter/nvim-treesitter'},
        config = function () LOAD_CONFIG('nav.tabout') end,
    }

    use {
        'nvim-telescope/telescope.nvim',
        tag = '0.1.0',
        module = 'telescope',
        cmd = 'Telescope',
        requires = {
            {'nvim-lua/plenary.nvim'},
            {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'},
            {'nvim-telescope/telescope-hop.nvim'},
            {'kyazdani42/nvim-web-devicons'},
        },
        config = function () LOAD_CONFIG('nav.telescope') end,
    }
    use {
        'ThePrimeagen/harpoon',
        requires = { {'nvim-lua/plenary.nvim'} },
        config = function ()
            LOAD_CONFIG('nav.harpoon')
            LOAD_MAPPING('nav.harpoon')
        end,
    }

    use {
        'stevearc/aerial.nvim',
        requires = { 'nvim-treesitter/nvim-treesitter' },
        config = function ()
            LOAD_CONFIG('nav.aerial')
            LOAD_MAPPING('nav.aerial')
        end,
    }

    ----------------------------------------------------------------------------
    -- Text Parsing
    ----------------------------------------------------------------------------
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        requires = {
            {'nvim-treesitter/nvim-treesitter-textobjects'},
            {'RRethy/nvim-treesitter-textsubjects'},
            {'JoosepAlviste/nvim-ts-context-commentstring'},
            {'RRethy/nvim-treesitter-endwise'},
            {'windwp/nvim-ts-autotag'},
            {'nvim-treesitter/playground', cmd = 'TSPlaygroundToggle'},
        },
        config = function ()
            LOAD_CONFIG('treesitter')
            LOAD_MAPPING('treesitter')
        end,
    }

    ----------------------------------------------------------------------------
    -- LSP
    ----------------------------------------------------------------------------
    use {
        'williamboman/mason.nvim',
        config = function () LOAD_CONFIG('misc.mason') end,
    }
    use {
        'williamboman/mason-lspconfig.nvim',
        after = 'mason.nvim',
        requires = {'williamboman/mason-lspconfig.nvim'},
        config = function () LOAD_CONFIG('lsp.mason') end,
    }
    use {
        'neovim/nvim-lspconfig',
        requires = {
            {'williamboman/mason.nvim'},
            {'williamboman/mason-lspconfig.nvim'},
        },
        after = 'mason-lspconfig.nvim',
        config = function ()
            LOAD_CONFIG('lsp.config')
            LOAD_MAPPING('lsp.config')
        end,
    }
    use {
        'ray-x/lsp_signature.nvim',
        config = function () LOAD_CONFIG('lsp.signature') end,
    }

    ----------------------------------------------------------------------------
    -- Autocompletion (nvim-cmp)
    ----------------------------------------------------------------------------
    use {
        'hrsh7th/nvim-cmp',
        opt = true,
        config = function ()
            LOAD_CONFIG('lsp.cmp')
            LOAD_MAPPING('lsp.cmp')
        end,
    }

    use {'onsails/lspkind.nvim', module = 'lspkind'}
    use {'hrsh7th/cmp-nvim-lsp', after = 'nvim-cmp'}
    use {'hrsh7th/cmp-buffer', after = 'nvim-cmp'}
    use {'hrsh7th/cmp-path', after = 'nvim-cmp'}
    use {'hrsh7th/cmp-cmdline', after = 'nvim-cmp'}
    use {'hrsh7th/cmp-nvim-lua', after = 'nvim-cmp'}
    use {'saadparwaiz1/cmp_luasnip', after = {'nvim-cmp', 'LuaSnip'}}

    ----------------------------------------------------------------------------
    -- Snippets
    ----------------------------------------------------------------------------
    use {
        'L3MON4D3/LuaSnip',
        module = 'luasnip',
        requires = {
            {'rafamadriz/friendly-snippets'}
        },
        config = function () LOAD_CONFIG('lsp.luasnip') end,
    }

    -- install all plugins on first load
    if firstload then
        require('packer').sync()
    end
end)
