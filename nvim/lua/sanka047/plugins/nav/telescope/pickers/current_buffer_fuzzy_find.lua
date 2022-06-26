--------------------------------------------------------------------------------
-- Current Buffer Fuzzy Find Picker
--------------------------------------------------------------------------------
return {
    layout_strategy = 'vertical',
    layout_config = {
        prompt_position = 'bottom',
        height = 0.9,
        width = function (_, max_columns, _)
            local width = math.ceil(max_columns * 0.8)
            if width > 120 then
                width = 120
            end
            return width
        end,
        preview_height = function (_, _, max_lines)
            local height = math.ceil(max_lines * 0.3)
            if height < 5 then
                height = 5
            elseif height > 15 then
                height = 15
            end

            return height
        end
    },
}
