--------------------------------------------------------------------------------
-- Custom NumpyDoc
--------------------------------------------------------------------------------
-- Removed most of the default docstrings on arguments. If these are desired, user can add them in
-- later. Most variables are named appropriately and meaning can be understood from the surrounding
-- context.
--
-- Extra new lines are annoying to deal with/clean up after the fact
local i = require("neogen.types.template").item

local type_sep = ': ' -- work drops the first space (pulling this out in case I need to add back in)
local cls_doc_sep = '' -- modify to conform later, but work again

return {
    {
        nil,
        '"""' .. cls_doc_sep .. '$1' .. cls_doc_sep .. '"""',
        { no_results = true, type = { "class", "func" } },
    },
    { nil, '"""$1', { no_results = true, type = { "file" } } },
    { nil, "", { no_results = true, type = { "file" } } },
    { nil, "$1", { no_results = true, type = { "file" } } },
    { nil, '"""', { no_results = true, type = { "file" } } },
    { nil, "", { no_results = true, type = { "file" } } },

    { nil, "# $1", { no_results = true, type = { "type" } } },

    { nil, '"""$1' },
    { i.HasParameter, "", { type = { "func" } } },
    { i.HasParameter, "Parameters", { type = { "func" } } },
    { i.HasParameter, "----------", { type = { "func" } } },
    {
        i.Parameter,
        "%s" .. type_sep .. "$1",
        { type = { "func" } },
    },
    {
        { i.Parameter, i.Type },
        "%s" .. type_sep .. "%s$1",
        { required = i.Tparam, type = { "func" } },
    },
    {
        i.ArbitraryArgs,
        "%s",
        { after_each = "    $1", type = { "func" } },
    },
    {
        i.Kwargs,
        "%s",
        { after_each = "    $1", type = { "func" } },
    },
    { i.ClassAttribute, "%s" .. type_sep .. "$1", { before_first_item = { "", "Attributes", "----------" } } },
    { i.HasReturn, "", { type = { "func" } } },
    { i.HasReturn, "Returns", { type = { "func" } } },
    { i.HasReturn, "-------", { type = { "func" } } },
    { i.ReturnTypeHint, "%s$1" },
    { i.Return, "$1" },
    { nil, '"""' },
}
