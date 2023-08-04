-- Need to add this to the language server to broadcast snippet compatibility
local capabilities = require("cmp_nvim_lsp").default_capabilities()

-- Dhall
require 'lspconfig'.dhall_lsp_server.setup {}

-- elm
require 'lspconfig'.elmls.setup {}

-- go
require 'lspconfig'.gopls.setup {
    capabilities = capabilities,
    settings = {
        gopls = {
            completeUnimported = true,
            usePlaceholders = true,
            analyses = {
                unusedparams = true,
            }
        }
    }
}

-- JavaScript/TypeScript
require 'lspconfig'.tsserver.setup {}

-- lua
require 'lspconfig'.lua_ls.setup {
    settings = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using
                -- (most likely LuaJIT in the case of Neovim)
                version = 'LuaJIT',
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = { 'vim' },
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = vim.api.nvim_get_runtime_file("", true),
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {
                enable = false,
            },
        },
    },
}

-- nix
require 'lspconfig'.nil_ls.setup {}

-- Python
require 'lspconfig'.pyright.setup { capabilities = capabilities }

-- SQL
require('lspconfig').sqls.setup {
    on_attach = function(client, bufnr)
        require('sqls').on_attach(client, bufnr)
    end
}

-- Terraform
require 'lspconfig'.terraform_lsp.setup {}

-- shit you need to deal with
require 'lspconfig'.bashls.setup {}
require 'lspconfig'.cmake.setup {}
require 'lspconfig'.cssls.setup { capabilities = capabilities }
require 'lspconfig'.dockerls.setup {}
require 'lspconfig'.html.setup { capabilities = capabilities }

-- Grammer
require 'lspconfig'.ltex.setup {}

-- null (Various tools as LSP) Setup
local null_ls = require("null-ls")
null_ls.setup {
    sources = {
        -- grammer
        null_ls.builtins.diagnostics.vale,
        -- markdown
        null_ls.builtins.diagnostics.markdownlint,
        null_ls.builtins.formatting.markdownlint,
        -- python
        null_ls.builtins.formatting.black,
        null_ls.builtins.diagnostics.mypy,
        null_ls.builtins.diagnostics.ruff,
        -- js, html, css, formatting
        null_ls.builtins.formatting.prettier,
        -- nix
        null_ls.builtins.code_actions.statix,
        null_ls.builtins.diagnostics.statix,
        null_ls.builtins.diagnostics.deadnix,
        null_ls.builtins.formatting.nixfmt,
        -- shell scripting
        null_ls.builtins.code_actions.shellcheck,
        -- terraform
        null_ls.builtins.formatting.terraform_fmt,
        -- other
        null_ls.builtins.diagnostics.gitlint,
        null_ls.builtins.diagnostics.hadolint,
        null_ls.builtins.diagnostics.yamllint,
        -- go formatting
        null_ls.builtins.formatting.gofmt,
    }
}


-- Basic LSP keybindings
-- Jump to Definition/Implementation
vim.api.nvim_set_keymap(
    'n', 'gd', [[<cmd>lua vim.lsp.buf.definition()<CR>]],
    { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
    'n', 'gi', [[<cmd>lua vim.lsp.buf.implementation()<CR>]],
    { noremap = true, silent = true }
)
