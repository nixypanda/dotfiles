-- Need to add this to the language server to broadcast snippet compatibility
local capabilities = require("cmp_nvim_lsp").default_capabilities()

-- Clojure
require 'lspconfig'.clojure_lsp.setup {}

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

-- grammer
require 'lspconfig'.ltex.setup {
    settings = {
        ltex = {
            ['ltex-ls'] = {
                logLevel = "warning"
            }
        }
    }
}

-- Java
require 'lspconfig'.java_language_server.setup { cmd = { 'java-language-server' } }

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
-- require('lspconfig').sqls.setup {
--     on_attach = function(client, bufnr)
--         require('sqls').on_attach(client, bufnr)
--     end
-- }

-- Terraform
require 'lspconfig'.terraform_lsp.setup {}

-- shit you need to deal with
require 'lspconfig'.bashls.setup {}
require 'lspconfig'.cmake.setup {}
require 'lspconfig'.cssls.setup { capabilities = capabilities }
require 'lspconfig'.dockerls.setup {}
require 'lspconfig'.html.setup { capabilities = capabilities }

local lsp_mypy = require('lint').linters.mypy
lsp_mypy.cmd = function()
    local return_code = os.execute("poetry run mypy --version 1>/dev/null 2>/dev/null")
    if return_code == 0 then
        return "venv-mypy"
    end
    return "mypy"
end

require('lint').linters_by_ft = {
    markdown = { 'vale', 'markdownlint' },
    python = { 'mypy', 'ruff' },
    nix = { 'statix' },
    bash = { 'shellcheck' },
    sh = { 'shellcheck' },
    dockerfile = { 'hadolint' },
    yaml = { 'yamllint' },
}

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
    callback = function()
        require("lint").try_lint()
    end,
})


-- LSP File operations
require("lsp-file-operations").setup()

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
