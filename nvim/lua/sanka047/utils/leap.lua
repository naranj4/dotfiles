--------------------------------------------------------------------------------
-- Leap Config
--------------------------------------------------------------------------------
local leap = {}

leap.direction = {
    BACKWARD = 'backward',
    FORWARD = 'forward',
}

local function get_first_non_whitespace(line)
    -- regex line and col is 0 indexed
    return (vim.regex('\\S'):match_line(0, line - 1) or 0) + 1
end

function leap.get_first_non_whitespace_lines(winid, direction)
    local wininfo = vim.fn.getwininfo(winid)[1]
    local cur_line = unpack(vim.api.nvim_win_get_cursor(winid))

    local start, final = cur_line, wininfo.botline
    if direction == leap.direction.BACKWARD then
        start, final = wininfo.topline, cur_line
    end

    -- list of { row, col } coordinates
    local targets = {}
    for ln = start, final do
        table.insert(targets, { pos = { ln, get_first_non_whitespace(ln) } })
    end

    -- sort by distance from cursor
    local function sort_by_distance_from_cursor(target)
        return math.abs(cur_line - target.pos[1])
    end
    table.sort(targets, function (t1, t2)
        return sort_by_distance_from_cursor(t1) < sort_by_distance_from_cursor(t2)
    end)

    if #targets >= 1 then return targets end
end

function leap.leap_line(direction)
    local winid = vim.api.nvim_get_current_win()

    require('leap').leap({
        targets = leap.get_first_non_whitespace_lines(winid, direction),
        target_windows = { winid },
    })
end

return leap
