-- Need to add this to the language server to broadcast snippet compatibility
local capabilities = require("cmp_nvim_lsp").default_capabilities()
local lspconfig = require("lspconfig")

-- go
lspconfig.gopls.setup({
	capabilities = capabilities,
	settings = {
		gopls = {
			completeUnimported = true,
			usePlaceholders = true,
			analyses = {
				unusedparams = true,
			},
		},
	},
})

-- grammer
lspconfig.ltex.setup({
	settings = {
		ltex = {
			["ltex-ls"] = {
				logLevel = "warning",
			},
		},
	},
})

-- Java
lspconfig.java_language_server.setup({ cmd = { "java-language-server" } })

-- JavaScript/TypeScript
lspconfig.tsserver.setup({})

-- lua
lspconfig.lua_ls.setup({
	settings = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using
				-- (most likely LuaJIT in the case of Neovim)
				version = "LuaJIT",
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { "vim" },
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
})

-- nix
lspconfig.nil_ls.setup({})

-- nu
lspconfig.nushell.setup({})

-- Python
lspconfig.pyright.setup({ capabilities = capabilities })

-- SQL
-- require('lspconfig').sqls.setup {
--     on_attach = function(client, bufnr)
--         require('sqls').on_attach(client, bufnr)
--     end
-- }

-- Terraform
lspconfig.terraform_lsp.setup({})

-- shit you need to deal with
lspconfig.bashls.setup({})
lspconfig.cmake.setup({})
lspconfig.cssls.setup({ capabilities = capabilities })
lspconfig.dockerls.setup({})
lspconfig.html.setup({ capabilities = capabilities })

-- Basic LSP keybindings
-- Jump to Definition/Implementation
vim.api.nvim_set_keymap("n", "gd", [[<cmd>lua vim.lsp.buf.definition()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "gi", [[<cmd>lua vim.lsp.buf.implementation()<CR>]], { noremap = true, silent = true })

-- Add border to lspconfig info screen
local lspconfig_window = require("lspconfig.ui.windows")
local old_defaults = lspconfig_window.default_opts

function lspconfig_window.default_opts(opts)
	local win_opts = old_defaults(opts)
	win_opts.border = "rounded"
	return win_opts
end

-- json/yaml configs
local json_schemas = {
	{
		description = "TypeScript compiler configuration file",
		fileMatch = { "tsconfig.json", "tsconfig.*.json" },
		url = "https://json.schemastore.org/tsconfig.json",
	},
	{
		description = "Babel configuration",
		fileMatch = { ".babelrc.json", ".babelrc", "babel.config.json" },
		url = "https://json.schemastore.org/babelrc.json",
	},
	{
		description = "ESLint config",
		fileMatch = { ".eslintrc.json", ".eslintrc" },
		url = "https://json.schemastore.org/eslintrc.json",
	},
	{
		description = "Prettier config",
		fileMatch = { ".prettierrc", ".prettierrc.json", "prettier.config.json" },
		url = "https://json.schemastore.org/prettierrc",
	},
	{
		description = "AWS CloudFormation",
		fileMatch = { "*.cf.json", "cloudformation.json" },
		url = "https://raw.githubusercontent.com/awslabs/goformation/v5.2.9/schema/cloudformation.schema.json",
	},
	{
		description = "Json schema for properties json file for a GitHub Workflow template",
		fileMatch = { ".github/workflow-templates/**.properties.json" },
		url = "https://json.schemastore.org/github-workflow-template-properties.json",
	},
	{
		description = "golangci-lint configuration file",
		fileMatch = { ".golangci.toml", ".golangci.json" },
		url = "https://json.schemastore.org/golangci-lint.json",
	},
	{
		description = "NPM configuration file",
		fileMatch = { "package.json" },
		url = "https://json.schemastore.org/package.json",
	},
}

lspconfig.jsonls.setup({ settings = { json = { schemas = json_schemas } } })

local yaml_schemas = {
	["https://json.schemastore.org/github-workflow.json"] = "/.github/workflows/*",
	["https://json.schemastore.org/drone.json"] = "/.drone.yml",
	["https://raw.githubusercontent.com/OAI/OpenAPI-Specification/main/schemas/v3.1/schema.json"] = "/openapi.yml",
	["https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json"] = "/docker-compose.json",
}

lspconfig.yamlls.setup({
	settings = {
		yaml = {
			schemas = yaml_schemas,
		},
	},
})
