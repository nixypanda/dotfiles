local formats = {
    "*.go", "*.hs", "*.json", "*.lua", "*.md", "*.nix", "*.py", "*.rs", "*.tf", "*.yaml",
}

for _, file_pattern in ipairs(formats) do
    vim.api.nvim_create_autocmd(
        'BufWritePre',
        {
            pattern = file_pattern,
            callback = function() vim.lsp.buf.format(nil) end
        }
    )
end
