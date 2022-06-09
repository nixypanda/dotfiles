local file_syntax_map = {
    { pattern = "*.rasi", syntax = "scss" },
    { pattern = "flake.lock", syntax = "json" },
    { pattern = "*.tfstate", syntax = "json" },
    { pattern = "*.nomad", syntax = "hcl" },
}

for _, elm in ipairs(file_syntax_map) do
    vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
        pattern = elm.pattern,
        command = "set syntax=" .. elm.syntax,
    })
end
