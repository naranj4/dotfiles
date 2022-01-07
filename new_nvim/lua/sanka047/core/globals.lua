--------------------------------------------------------------------------------
-- Define Useful Global Functions
--------------------------------------------------------------------------------
P = function(v)
  print(vim.inspect(v))
  return v
end

RELOAD = function(...)
  return require("plenary.reload").reload_module(...)
end

R = function(name)
  RELOAD(name)
  return require(name)
end

LOAD_CONFIG = function(plugin)
    pcall(require, 'sanka047.plugins.configs.' .. plugin)
end

LOAD_MAPPING = function(plugin)
    pcall(require, 'sanka047.plugins.mappings.' .. plugin)
end
