local file_syntax_map = {
    { pattern = "*.rasi", syntax = "scss" },
    { pattern = "flake.lock", syntax = "json" },
    { pattern = "*.tfstate", syntax = "json" },
    { pattern = "*.nomad", syntax = "hcl" },
    { pattern = "lotus58.keymap", syntax = "c" },
    { pattern = "lotus58.conf", syntax = "c" },
}

for _, elm in ipairs(file_syntax_map) do
    vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
        pattern = elm.pattern,
        command = "set syntax=" .. elm.syntax,
    })
end

require("headlines").setup()

-- This is a very specific setting which sets the color of border to be the
-- dark background of the tokyonight theme. This is done because we want to
-- have a seemless split between the filetree plugin and the buffer. As a side
-- effect for this we also get prominent splits as we need to use huge
-- forground blocks to set this up.
vim.g.tokyonight_colors = {
    border = '#1f2335'
}
