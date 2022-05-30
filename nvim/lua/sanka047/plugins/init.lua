--------------------------------------------------------------------------------
-- Plugins
--------------------------------------------------------------------------------
local firstload = require('sanka047.plugins.firstload')

if firstload then
    return false
end

local has_impatient, impatient = pcall(require, 'impatient')
if has_impatient then
    impatient.enable_profile()
end

local packer = require('packer')

packer.init({
    display = {
        open_fn = function()
            return require('packer.util').float { border = 'rounded' }
        end,
        prompt_border = 'rounded',
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
local log = require('sanka047.utils.log')
local registry = require('sanka047.registry')

local function _lazy_load_condition(metadata)
    log.debug('Metadata: ' .. vim.inspect(metadata))
    local ignore_filetypes = { TelescopePrompt = true, DressingInput = true }

    local ft = vim.bo[metadata.buf].filetype
    if ignore_filetypes[ft] then
        log.debug('Not lazy loading plugins in filetype `' .. ft .. '`')
        return false
    end

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
    ['nvim-cmp'] = 'InsertEnter',
}
for plugin, event in pairs(lazy_loaded) do
    lazy_load_on_event(event, plugin)
end

--------------------------------------------------------------------------------
-- Plugin List
--------------------------------------------------------------------------------
return require('packer').startup(function(use)
    use {'lewis6991/impatient.nvim'}
    use {'dstein64/vim-startuptime'}
    use {'nathom/filetype.nvim'}
    use {
        'antoinemadec/FixCursorHold.nvim',
        config = function () vim.g.cursorhold_updatetime = 100 end,
    }
    use {'nvim-lua/plenary.nvim'}

    -- let packer manager itself
    use {'wbthomason/packer.nvim'}

    -- Document Mapping
    use {
        'folke/which-key.nvim',
        config = function () LOAD_CONFIG('which-key') end,
    }

    -- Color Visualization
    use {
        'norcalli/nvim-colorizer.lua',
        cmd = 'ColorizerToggle',
        config = function () LOAD_CONFIG('colorizer') end,
    }

    -- Colorschemes
    use {
        'tomasiser/vim-code-dark',
        config = function () LOAD_CONFIG('colors.code-dark') end,
    } -- pretty nice and visible
    use {
        'rebelot/kanagawa.nvim',
        config = function () LOAD_CONFIG('colors.kanagawa') end,
    } -- nice, nice highlighting, no real complaints off the top of my head, nice completion menu
    use {'savq/melange'}

    -- Aesthetics & UI
    use {
        'kyazdani42/nvim-web-devicons',
        config = function () LOAD_CONFIG('nvim-web-devicons') end,
    }
    use {
        'stevearc/dressing.nvim',
        config = function () LOAD_CONFIG('dressing') end,
    }
    use {
        'rcarriga/nvim-notify',
        config = function () LOAD_CONFIG('notify') end,
    }
    use {
        'j-hui/fidget.nvim',
        config = function () LOAD_CONFIG('fidget') end,
    }

    use {
        'sanka047/Shade.nvim',
        config = function ()
            LOAD_CONFIG('shade')
            LOAD_MAPPING('shade')
        end,
    }

    use {
        'edluffy/specs.nvim',
        config = function () LOAD_CONFIG('specs') end,
    }
    use {
        'RRethy/vim-illuminate',
        config = function () LOAD_CONFIG('illuminate') end,
    }

    use {
        'lukas-reineke/indent-blankline.nvim',
        config = function () LOAD_CONFIG('indent-blankline') end,
    }
    use {
        'nvim-lualine/lualine.nvim',
        requires = { {'kyazdani42/nvim-web-devicons'} },
        config = function () LOAD_CONFIG('lualine') end,
    }
    use {
        'b0o/incline.nvim',
        config = function () LOAD_CONFIG('incline') end,
    }
    use {
        'SmiteshP/nvim-gps',
        config = function () LOAD_CONFIG('nvim-gps') end,
    }

    use {
        'glepnir/dashboard-nvim',
        requires = { {'telescope.nvim'} },
        config = function ()
            LOAD_CONFIG('dashboard')
            LOAD_MAPPING('dashboard')
        end,
    }

    use {
        'kyazdani42/nvim-tree.lua',
        requires = { {'kyazdani42/nvim-web-devicons'} },
        cmd = {'NvimTreeToggle', 'NvimTreeFindFileToggle'},
        config = function ()
            LOAD_CONFIG('nvim-tree')
            LOAD_MAPPING('nvim-tree')
        end,
    }

    -- Git
    use {
        'lewis6991/gitsigns.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        config = function () LOAD_CONFIG('gitsigns') end,
    }
    use {
        'tpope/vim-fugitive',
        config = function () LOAD_MAPPING('fugitive') end,
    }
    use {
        'sindrets/diffview.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        commit = '08e4340f690d0b611a393eafb633b2fb62f78601',
        config = function ()
            LOAD_CONFIG('diffview')
            LOAD_MAPPING('diffview')
        end,
    }
    use {
        'TimUntersberger/neogit',
        requires = { {'sindrets/diffview.nvim'} },
        module = 'neogit',
        config = function () LOAD_CONFIG('neogit') end,
    }

    -- Floating terminal (for ease of use with git)
    use {
        'akinsho/toggleterm.nvim',
        config = function ()
            LOAD_CONFIG('toggleterm')
            LOAD_MAPPING('toggleterm')
        end,
    }

    -- Editing Magic
    use {
        'windwp/nvim-autopairs',
        opt = true,
        config = function ()
            LOAD_CONFIG('nvim-autopairs')
            LOAD_MAPPING('nvim-autopairs')
        end,
    }
    use {
        'machakann/vim-sandwich',
        config = function ()
            LOAD_CONFIG('sandwich')
            LOAD_MAPPING('sandwich')
        end,
    }
    use {
        'danymat/neogen',
        module = 'neogen',
        requires = 'nvim-treesitter/nvim-treesitter',
        config = function () LOAD_CONFIG('neogen') end,
    }
    use {
        'numToStr/Comment.nvim',
        module = 'Comment',
        config = function () LOAD_CONFIG('comment') end,
    }
    use {
        'ThePrimeagen/refactoring.nvim',
        module = 'sanka047.utils.refactor',
        requires = {
            {'nvim-lua/plenary.nvim'},
            {'nvim-treesitter/nvim-treesitter'},
        },
        config = function () LOAD_CONFIG('refactoring') end,
    }

    -- Movement & File Navigation
    use {
        'nacro90/numb.nvim',
        config = function () LOAD_CONFIG('numb') end,
    }
    use {
        'phaazon/hop.nvim',
        branch = 'v1',
        module = 'hop',
        config = function () LOAD_CONFIG('hop') end,
    }
    use {'unblevable/quick-scope'}
    use {
        'abecodes/tabout.nvim',
        requires = {'nvim-treesitter/nvim-treesitter'},
        config = function () LOAD_CONFIG('tabout') end,
    }

    use {
        'nvim-telescope/telescope.nvim',
        module = 'telescope',
        cmd = 'Telescope',
        requires = {
            {'nvim-lua/plenary.nvim'},
            {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'},
            {'nvim-telescope/telescope-hop.nvim'},
            {'kyazdani42/nvim-web-devicons'},
        },
        config = function () LOAD_CONFIG('telescope') end,
    }

    -- Text Substitution/Processing
    use {'tpope/vim-abolish'}
    use {
        'gbprod/substitute.nvim',
        module = 'substitute',
        config = function () LOAD_CONFIG('substitute') end,
    }

    -- Text Parsing
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        requires = {
            {'nvim-treesitter/nvim-treesitter-textobjects'},
            {'RRethy/nvim-treesitter-textsubjects'},
            {'JoosepAlviste/nvim-ts-context-commentstring'},
            {'RRethy/nvim-treesitter-endwise'},
            {'windwp/nvim-ts-autotag'},
            {
                'nvim-treesitter/playground',
                cmd = 'TSPlaygroundToggle',
            },
        },
        config = function ()
            LOAD_CONFIG('treesitter')
            LOAD_MAPPING('treesitter')
        end,
    }
    use {
        'romgrk/nvim-treesitter-context',
        requires = {'nvim-treesitter/nvim-treesitter'},
        config = function () LOAD_CONFIG('treesitter-context') end,
    }

    -- Snippets
    use {
        'L3MON4D3/LuaSnip',
        module = 'luasnip',
        requires = {
            {'rafamadriz/friendly-snippets'}
        },
        config = function () LOAD_CONFIG('luasnip') end,
    }

    -- LSP and Autocompletion
    use {
        'williamboman/nvim-lsp-installer',
        requires = {
            {'neovim/nvim-lspconfig'},
            {
                'ray-x/lsp_signature.nvim',
                config = function () LOAD_CONFIG('lsp-signature') end,
            },
        },
        config = function ()
            LOAD_MAPPING('lsp-installer')
            LOAD_CONFIG('lsp-installer')
        end,
    }

    -- Autocompletion (nvim-cmp)
    use {
        'hrsh7th/nvim-cmp',
        opt = true,
        config = function ()
            LOAD_CONFIG('nvim-cmp')
            LOAD_MAPPING('nvim-cmp')
        end,
    }

    use {'onsails/lspkind.nvim', module = 'lspkind'}
    use {'hrsh7th/cmp-nvim-lsp', after = 'nvim-cmp'}
    use {'hrsh7th/cmp-buffer', after = 'nvim-cmp'}
    use {'hrsh7th/cmp-path', after = 'nvim-cmp'}
    use {'hrsh7th/cmp-cmdline', after = 'nvim-cmp'}
    use {'hrsh7th/cmp-nvim-lua', after = 'nvim-cmp'}
    use {'saadparwaiz1/cmp_luasnip', after = {'nvim-cmp', 'LuaSnip'}}

    -- Autocompletion (coq_nvim)
    -- TODO: To avoid the problems with mapping, drop the snippets from coq and instead use null-ls to
    -- inject luasnip as an LSP source. Then use Luasnip for flexible mappings and the rest of coq's
    -- defaults should work fine.
    -- use {
    --     'ms-jpq/coq_nvim',
    --     branch = 'coq', after = 'nvim-web-devicons',
    --     requires = {
    --         {'ms-jpq/coq.artifacts', branch = 'artifacts'},  -- snippets
    --         {'ms-jpq/coq.thirdparty', branch = '3p'},
    --     },
    -- }
end)
