vim.api.nvim_set_option('laststatus', 3)

require('lualine').setup {
    options = {
        section_separators = { left = '', right = '' },
        component_separators = { left = '', right = '' },
        disabled_filetypes = { "NvimTree" },
        iconsEnabled = true
    },
    sections = {
        lualine_a = { { 'mode', upper = true } },
        lualine_b = { 'filename' },
        lualine_c = {
            {
                'diagnostics',
                sources = { 'nvim_diagnostic' },
                symbols = { error = " ", warn = " ", info = "", hint = " " },
            }
        },
        lualine_x = { 'diff', 'branch' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' }
    }
}

-- Workaround to make the global statusline look shifted over when nvim tree is
-- active
local nvim_tree_shift = {
    function()
        return string.rep(
            ' ', vim.api.nvim_win_get_width(require 'nvim-tree.view'.get_winnr()) - 1
        )
    end,
    cond = require('nvim-tree.view').is_visible,
    color = 'BufferInactive'
}

require 'lualine'.setup {
    sections = {
        lualine_a = { nvim_tree_shift, "mode" }
    },
}
