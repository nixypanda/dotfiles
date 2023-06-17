local capabilities = require("cmp_nvim_lsp").default_capabilities()
local rt = require("rust-tools")
local injected_config = require("injected")

rt.setup({
    tools = {
        hover_actions = { auto_focus = true },
    },
    server = {
        capabilities = capabilities,
        settings = {
            ["rust-analyzer"] = {
                checkOnSave = { command = "clippy" },
                -- RA will scan .direnv correspondingly entire nixpkgs repository
                files = { excludeDirs = { ".direnv" } }
            }
        }
    },
    dap = {
        adapter = require('rust-tools.dap').get_codelldb_adapter(
            injected_config.codelldb_path,
            injected_config.liblldb_path
        )
    }
})
require 'crates'.setup()
