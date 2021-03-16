local gl = require('galaxyline')
local gls = gl.section
local devicons = require 'nvim-web-devicons'

local theme = {
    black       = '#282C34',
    black_light = '#3E4452',
    yellow      = "#d19a66",
    cyan        = "#56b6c2",
    darkblue    = '#081633',
    green       = "#98C379",
    magenta     = "#C678DD",
    grey        = "#e6efff",
    blue        = "#61AFEF",
    red         = "#E06C75",
}

local colors = {
    bg = theme.black,
    bgl = theme.black_light,
    fg = theme.white,

    normal = theme.green,
    insert = theme.blue,
    replace = theme.red,
    visual = theme.magenta,
    command = theme.yellow,
    terminal = theme.cyan,

    info = theme.blue,
    warn = theme.yellow,
    success = theme.green,
    error = theme.red,
}

local mode_map = {
    ['n'] = {'NORMAL', colors.normal},
    ['i'] = {'INSERT', colors.insert},
    ['R'] = {'REPLACE', colors.replace},
    ['v'] = {'VISUAL', colors.visual},
    ['V'] = {'V-LINE', colors.visual},
    ['c'] = {'COMMAND', colors.command},
    ['s'] = {'SELECT', colors.visual},
    ['S'] = {'S-LINE', colors.visual},
    ['t'] = {'TERMINAL', colors.terminal},
    [''] = {'V-BLOCK', colors.visual},
    [''] = {'S-BLOCK', colors.visual},
    ['Rv'] = {'VIRTUAL'},
    ['rm'] = {'--MORE'},
}

local icons = {
    locker = '',
    unsaved = '',
}

local buffer_not_empty = function()
    if vim.fn.empty(vim.fn.expand('%:t')) ~= 1 then
        return true
    end
    return false
end

local checkwidth = function()
    local squeeze_width  = vim.fn.winwidth(0) / 2
    if squeeze_width > 40 then
        return true
    end
    return false
end

local function mode_label() return mode_map[vim.fn.mode()][1] or 'N/A' end
local function mode_hl() return mode_map[vim.fn.mode()][2] or colors.none end

local function highlight(group, fg, bg, gui)
    local cmd = string.format('highlight %s guifg=%s guibg=%s', group, fg, bg)
    if gui ~= nil then cmd = cmd .. ' gui=' .. gui end
    vim.cmd(cmd)
end


local function wide_enough()
    local squeeze_width = vim.fn.winwidth(0)
    if squeeze_width > 80 then return true end
    return false
end

gl.short_line_list = {'NvimTree'}

local sep = { right_filled = '', left_filled = '', right = '', left = '' }

gls.left = {
    {
        ViMode = {
            provider = function()
                local modehl = mode_hl()
                highlight('GalaxyViMode', colors.bg, modehl, 'bold')
                highlight('GalaxyViModeInv', modehl, colors.bgl, 'bold')
                return string.format('  %s ', mode_label())
            end,
            separator = sep.right_filled,
            separator_highlight = 'GalaxyViModeInv',
        },
    },
    {
        FileIcon = {
            provider = function()
                local fname, ext = vim.fn.expand '%:t', vim.fn.expand '%:e'
                local icon, iconhl = devicons.get_icon(fname, ext)
                if icon == nil then return '' end
                local fg = vim.fn.synIDattr(vim.fn.hlID(iconhl), 'fg')
                highlight('GalaxyFileIcon', fg, colors.bgl)
                return ' ' .. icon .. ' '
            end,
            condition = buffer_not_empty,
        },
    },
    {
        FileName = {
            provider = function()
                if not buffer_not_empty() then return '' end
                local fname
                if wide_enough() then
                    fname = vim.fn.fnamemodify(vim.fn.expand '%', ':~:.')
                else
                    fname = vim.fn.expand '%:t'
                end
                if #fname == 0 then return '' end
                if vim.bo.readonly then fname = fname .. ' ' .. icons.locker end
                if vim.bo.modified then fname = fname .. ' ' .. icons.unsaved end
                return ' ' .. fname .. ' '
            end,
            highlight = {colors.fg, colors.bgl},
            separator = sep.right,
            separator_highlight = {colors.bg, colors.bgl},
        },
    },
    {
        DiagnosticError = {
            provider = 'DiagnosticError',
            icon = '  ',
            highlight = {colors.error,colors.bgl}
        }
    },
    {
        DiagnosticWarn = {
            provider = 'DiagnosticWarn',
            icon = '  ',
            highlight = {colors.warn,colors.bgl},
        }
    },
    {
        DiagnosticHint = {
            provider = 'DiagnosticHint',
            icon = '  ',
            highlight = {colors.info,colors.bgl},
        }
    },
    {
        DiagnosticInfo = {
            provider = 'DiagnosticInfo',
            icon = '  ',
            highlight = {colors.info,colors.bgl},
            separator = sep.right_filled,
            separator_highlight = {colors.bgl,colors.bg},
        }
    },
}


gls.right = {
    {
        DiffAdd = {
            provider = 'DiffAdd',
            condition = checkwidth,
            separator = sep.left_filled,
            separator_highlight = {colors.bgl,colors.bg},
            icon = '  ',
            highlight = {colors.success,colors.bgl},
        },
    },
    {
        DiffModified = {
            provider = 'DiffModified',
            condition = checkwidth,
            icon = '  ',
            highlight = {colors.warn,colors.bgl},
        },
    },
    {
        DiffRemove = {
            provider = 'DiffRemove',
            condition = checkwidth,
            icon = '  ',
            highlight = {colors.error,colors.bgl},
        },
    },
    {
        GitIcon = {
            provider = function() return ' ' end,
            condition = buffer_not_empty,
            separator = sep.left,
            separator_highlight = {colors.bg,colors.bgl},
            highlight = {colors.warn,colors.bgl},
        },
    },
    {
        GitBranch = {
            provider = 'GitBranch',
            condition = buffer_not_empty,
            highlight = {colors.fg,colors.bgl},
        },
    },
    {
        LineInfo = {
            provider = 'LineColumn',
            separator = sep.left_filled,
            separator_highlight = 'GalaxyViModeInv',
            highlight = 'GalaxyViMode',
        },
    },
    {
        PerCent = {
            provider = 'LinePercent',
            separator = ' |',
            separator_highlight = 'GalaxyViMode',
            highlight = 'GalaxyViMode',
        }
    },
}

gls.short_line_left = {
    {
        BufferType = {
            provider = 'FileTypeName',
            separator = sep.right,
            highlight = {colors.fg,colors.bgl},
            separator_highlight = {colors.fg,colors.bgl},
        },
    },
}
