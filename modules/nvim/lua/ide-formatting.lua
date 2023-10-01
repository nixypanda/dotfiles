require("conform").setup({
    formatters_by_ft = {
        css = { "prettier" },
        go = { "gofmt" },
        html = { "prettier" },
        javascript = { "prettier" },
        json = { "prettier" },
        lua = { "stylua" },
        markdown = { "prettier", "markdownlint" },
        nix = { "nixfmt" },
        python = { "isort", "black" },
        terraform = { "terraform_fmt" },
        yaml = { "prettier" }
    },
    format_on_save = {
        lsp_fallback = true,
        timeout_ms = 1000,
    },
})
