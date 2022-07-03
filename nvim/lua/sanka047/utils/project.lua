--------------------------------------------------------------------------------
-- Project Functions
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local create_autocmd = require('sanka047.utils.map').create_autocmd
local create_augroup = require('sanka047.utils.map').create_augroup

local Path = require('plenary.path')

local M = {}

local config = {
    directory = vim.fn.stdpath('config') .. '/projects',
}

M._augroup = nil
M._autocmd = nil

--- Sets up autocmd and augroup to automatically load the relevant config when in directory
function M.setup(opts)
    config = vim.tbl_extend("force", config, opts or {})
    M._augroup = create_augroup('ProjectConfiguration')
    M._autocmd = create_autocmd(
        'DirChanged',
        'Load project configuration for the new cwd',
        { group = M._augroup, callback = M.load_project_config }
    )

    -- call once at the start to ensure that initial project config is loaded
    M.load_project_config()
end

--- Gets the project-specific configuration directory
---@return Path directory where project-specific config is located
local function get_project_dir()
    local path = Path:new(config.directory)

    local project_path = vim.loop.cwd()

    -- replace all '/' that aren't alphanumeric with '-'
    local pconfig_dir = string.gsub(project_path, '/', '-')
    -- replace all characters that aren't alphanumeric (and '-') with '_'
    pconfig_dir = string.gsub(pconfig_dir, '[^%d%a-]', '_')

    path = path:joinpath(pconfig_dir)

    path:mkdir({ parents = true, exists_ok = true })

    return path
end

--- Get the path to the configuration file
---@param config_file string configuration file
---@return Path path to project-specific configuration file
local function get_project_config(config_file)
    local pdir = get_project_dir()

    return pdir:joinpath(config_file)
end

--- Load the configuration file
---@param config_file string configuration filename
---@return any result from calling pcall
function M.load_project_config_file(config_file)
    local pconfig_path = get_project_config(config_file)
    local abs_path = pconfig_path:absolute()
    log.debug('Load project config: ' .. abs_path, 'ProjectConfiguration')

    local ok, res = pcall(dofile, abs_path)
    if not ok then
        log.debug('Local project config load failed for: ' .. abs_path)
    end
    return res
end

--- Edit the configuration file
---@param config_file string configuration file
function M.edit_project_config_file(config_file)
    local pconfig_path = get_project_config(config_file)
    local abs_path = pconfig_path:absolute()
    log.debug('Edit project config: ' .. abs_path, 'ProjectConfiguration')

    vim.cmd('edit ' .. pconfig_path:absolute())
end

function M.load_project_config()
    return M.load_project_config_file('init.lua')
end

function M.edit_project_config()
    M.edit_project_config_file('init.lua')
    M.load_project_config()
end

return M
