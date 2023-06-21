-- prettier output for lsp diagnostics/renaming menu/references list/etc
require('lspsaga').setup({})

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    underline = true,
    update_in_insert = false,
    severity_sort = false
})

local diagnostic_symbol_map = {
    { name = "DiagnosticSignError", symbol = "☠ " },
    { name = "DiagnosticSignWarn",  symbol = " " },
    { name = "DiagnosticSignInfo",  symbol = "" },
    { name = "DiagnosticSignHint",  symbol = "☛ " },
}

for _, elm in ipairs(diagnostic_symbol_map) do
    vim.fn.sign_define(
        elm.name,
        { texthl = elm.name, text = elm.symbol, numhl = elm.name }
    )
end

-- Add border to lspconfig info screen
local lspconfig_window = require("lspconfig.ui.windows")
local old_defaults = lspconfig_window.default_opts

function lspconfig_window.default_opts(opts)
    local win_opts = old_defaults(opts)
    win_opts.border = "rounded"
    return win_opts
end

require('lspkind').init({})
