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
lspconfig.nixd.setup({})

-- nu
lspconfig.nushell.setup({})

-- Python
lspconfig.pyright.setup({ capabilities = capabilities })
-- We are primarily using pyright for everything. Only using pylsp for rope refactors.
lspconfig.pylsp.setup({
	settings = {
		pylsp = {
			plugins = {
				autopep8 = { enabled = false },
				jedi_completion = { enabled = false },
				jedi_definition = { enabled = false },
				jedi_hover = { enabled = false },
				jedi_references = { enabled = false },
				jedi_signature_help = { enabled = false },
				jedi_symbols = { enabled = false },
				maccabe = { enabled = false },
				preload = { enabled = false },
				pycodestyle = { enabled = false },
				pydocstyle = { enabled = false },
				pyflakes = { enabled = false },
				pylint = { enabled = false },
				rope_completion = { enabled = false },
				yapf = { enabled = false },
				-- using a fork of ropify for renameing modules etc,
				-- pyright already offers renaming capabilities that pylsp-rope offers at the moment
				pylsp_rope = { enabled = true, rename = false },
			},
		},
	},
})

-- SQL
lspconfig.sqls.setup({
	cmd = { "sqls", "-config", "~/.dotfiles/.secrets/nvim-sqls-config.yml" },

	on_attach = function(client, bufnr) require("sqls").on_attach(client, bufnr) end,
})

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

-- json/yaml/toml configs
lspconfig.jsonls.setup({
	settings = {
		json = {
			schemas = require("schemastore").json.schemas(),
			validate = { enable = true },
		},
	},
})

lspconfig.yamlls.setup({
	settings = {
		yaml = {
			schemaStore = {
				-- You must disable built-in schemaStore support if you want to use
				-- this plugin and its advanced options like `ignore`.
				enable = false,
				-- Avoid TypeError: Cannot read properties of undefined (reading 'length')
				url = "",
			},
			schemas = require("schemastore").yaml.schemas(),
		},
	},
})
-- already has SchemaStore configured
lspconfig.taplo.setup({})
