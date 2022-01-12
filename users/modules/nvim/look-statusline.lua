require('lualine').setup {
    options = {
        section_separators = {left = '', right = ''},
        component_separators = {left = '', right = ''},
        -- Hack : This is defined in the nvim/default.nix (String interpolation)
        theme = statusline_theme,
        disabled_filetypes = {"NvimTree"},
        iconsEnabled = true
    },
    sections = {
        lualine_a = {{'mode', upper = true}},
        lualine_b = {'filename'},
        lualine_c = {{'diagnostics', sources = {'nvim_diagnostic'}}},
        lualine_x = {'diff', 'branch'},
        lualine_y = {'progress'},
        lualine_z = {'location'}
    }
}
