-- Need to add this to the language server to broadcast snippet compatibility
local capabilities = require('cmp_nvim_lsp').default_capabilities()

-- Enable (broadcasting) snippet capability for completion
-- elm
require 'lspconfig'.elmls.setup {}

-- go
require 'lspconfig'.gopls.setup {}

-- Haskell
require 'lspconfig'.hls.setup {
    settings = { languageServerHaskell = { formattingProvider = "brittany" } }
}

-- JavaScript/TypeScript
require 'lspconfig'.tsserver.setup {}

-- lua
require 'lspconfig'.sumneko_lua.setup {
    cmd = { "lua-language-server" },
    settings = { Lua = { diagnostics = { globals = { 'vim' } } } }
}

-- nix
require 'lspconfig'.nil_ls.setup {}

-- Python
require 'lspconfig'.pyright.setup { capabilities = capabilities }

-- Rust
require 'lspconfig'.rust_analyzer.setup {
    capabilities = capabilities,
    on_attach = function()
        require 'lsp_signature'.on_attach({
            bind = true,
            handler_opts = { border = 'single' }
        })
    end
}
require 'crates'.setup()

-- SQL
require('lspconfig').sqls.setup {
    on_attach = function(client, bufnr)
        require('sqls').on_attach(client, bufnr)
    end
}

require 'lspconfig'.terraformls.setup {}

-- shit you need to deal with
require 'lspconfig'.bashls.setup {}
require 'lspconfig'.cmake.setup {}
require 'lspconfig'.cssls.setup { capabilities = capabilities }
require 'lspconfig'.dockerls.setup {}
require 'lspconfig'.html.setup { capabilities = capabilities }

local json_schemas = {
    {
        description = "TypeScript compiler configuration file",
        fileMatch = { "tsconfig.json", "tsconfig.*.json" },
        url = "https://json.schemastore.org/tsconfig.json"
    }, {
        description = "Babel configuration",
        fileMatch = { ".babelrc.json", ".babelrc", "babel.config.json" },
        url = "https://json.schemastore.org/babelrc.json"
    }, {
        description = "ESLint config",
        fileMatch = { ".eslintrc.json", ".eslintrc" },
        url = "https://json.schemastore.org/eslintrc.json"
    }, {
        description = "Prettier config",
        fileMatch = { ".prettierrc", ".prettierrc.json", "prettier.config.json" },
        url = "https://json.schemastore.org/prettierrc"
    }, {
        description = "Stylelint config",
        fileMatch = { ".stylelintrc", ".stylelintrc.json", "stylelint.config.json" },
        url = "https://json.schemastore.org/stylelintrc"
    }, {
        description = "Configuration file as an alternative for configuring your repository in the settings page.",
        fileMatch = { ".codeclimate.json" },
        url = "https://json.schemastore.org/codeclimate.json"
    }, {
        description = "AWS CloudFormation provides a common language for you to describe and provision all the infrastructure resources in your cloud environment.",
        fileMatch = { "*.cf.json", "cloudformation.json" },
        url = "https://raw.githubusercontent.com/awslabs/goformation/v5.2.9/schema/cloudformation.schema.json"
    }, {
        description = "The AWS Serverless Application Model (AWS SAM, previously known as Project Flourish) extends AWS CloudFormation to provide a simplified way of defining the Amazon API Gateway APIs, AWS Lambda functions, and Amazon DynamoDB tables needed by your serverless application.",
        fileMatch = { "serverless.template", "*.sam.json", "sam.json" },
        url = "https://raw.githubusercontent.com/awslabs/goformation/v5.2.9/schema/sam.schema.json"
    }, {
        description = "Json schema for properties json file for a GitHub Workflow template",
        fileMatch = { ".github/workflow-templates/**.properties.json" },
        url = "https://json.schemastore.org/github-workflow-template-properties.json"
    }, {
        description = "golangci-lint configuration file",
        fileMatch = { ".golangci.toml", ".golangci.json" },
        url = "https://json.schemastore.org/golangci-lint.json"
    }, {
        description = "NPM configuration file",
        fileMatch = { "package.json" },
        url = "https://json.schemastore.org/package.json"
    }
}

require 'lspconfig'.jsonls.setup { settings = { json = { schemas = json_schemas } } }
require 'lspconfig'.vimls.setup {}
require 'lspconfig'.yamlls.setup {
    settings = {
        yaml = {
            schemas = {
                ["https://json.schemastore.org/github-workflow.json"] = "/.github/workflows/*",
                ["https://json.schemastore.org/drone.json"] = "/.drone.yml",
                ["https://raw.githubusercontent.com/OAI/OpenAPI-Specification/main/schemas/v3.1/schema.json"] = "/openapi.yml",
                ["https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json"] = "/docker-compose.json"
            }
        }
    }
}

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
        null_ls.builtins.diagnostics.ruff.with({ command = { "python", "-m", "ruff" } }),
        -- js, html, css, formatting
        null_ls.builtins.formatting.prettier,
        -- nix
        null_ls.builtins.code_actions.statix,
        null_ls.builtins.diagnostics.statix,
        null_ls.builtins.diagnostics.deadnix,
        -- shell scripting
        null_ls.builtins.code_actions.shellcheck,
        -- other
        null_ls.builtins.diagnostics.gitlint,
        null_ls.builtins.diagnostics.hadolint,
        null_ls.builtins.diagnostics.yamllint,
    }
}


-- signature help
require 'lsp_signature'.on_attach({ bind = true, handler_opts = { border = 'single' } })


-- Basic LSP keybindings
-- Jump to Definition/Implementation
vim.api.nvim_set_keymap('n', 'gd', [[<cmd>lua vim.lsp.buf.definition()<CR>]],
    { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'gi', [[<cmd>lua vim.lsp.buf.implementation()<CR>]],
    { noremap = true, silent = true })
-- scroll down hover doc or scroll in definition preview
vim.api.nvim_set_keymap('n', '<C-f>',
    [[<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>]],
    { noremap = true, silent = true })
-- scroll up hover doc
vim.api.nvim_set_keymap('n', '<C-d>',
    [[<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>]],
    { noremap = true, silent = true })

-- AUTO FORMATTING

for _, file_pattern in ipairs({ "*.rs", "*.hs", "*.py", "*.lua", "*.md", "*.nix", "*.tf" }) do
    vim.api.nvim_create_autocmd(
        'BufWritePre',
        {
            pattern = file_pattern,
            callback = function() vim.lsp.buf.formatting_sync(nil, 1000) end
        }
    )
end

-- Prettify LSP diagnostic messages/icons

-- prettier output for lsp diagnostics/renaming menu/references list/etc
local saga = require 'lspsaga'
saga.init_lsp_saga {
    border_style = "rounded",
    move_in_saga = { prev = 'k', next = 'j' },
    diagnostic_header = { " ", " ", "", " " },
    code_action_icon = " ",
    finder_action_keys = {
        open = '<CR>',
        vsplit = 'v',
        split = 's',
        quit = { 'q', '<Esc>' }
    },
    code_action_keys = {
        quit = { "q", '<Esc>' },
        exec = "<CR>",
    },
}
require 'lspsaga.diagnostic'.show_line_diagnostics()

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    underline = true,
    update_in_insert = false,
    severity_sort = false
})

local diagnostic_symbol_map = {
    { name = "DiagnosticSignError", symbol = " " },
    { name = "DiagnosticSignWarn", symbol = " " },
    { name = "DiagnosticSignInfo", symbol = "" },
    { name = "DiagnosticSignHint", symbol = " " },
}

for _, elm in ipairs(diagnostic_symbol_map) do
    vim.fn.sign_define(elm.name, { texthl = elm.name, text = elm.symbol, numhl = elm.name })
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

-- Display LSP messages overlayd on the current buffer (instead of the status
-- line) at the bottom right corner
require "fidget".setup {}
