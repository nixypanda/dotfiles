require("lz.n").load({
	"nvim-lspconfig",
	event = { "BufReadPre", "BufNewFile" },
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("lspsaga.nvim")
		vim.cmd.packadd("SchemaStore.nvim")
	end,
	after = function()
		-- Add border to lspconfig info screen
		local lspconfig_window = require("lspconfig.ui.windows")
		local old_defaults = lspconfig_window.default_opts

		function lspconfig_window.default_opts(opts)
			local win_opts = old_defaults(opts)
			win_opts.border = "rounded"
			return win_opts
		end

		require("lspsaga").setup({
			symbol_in_winbar = { enable = false },
			lightbulb = { ignore = { clients = { "kulala" } } },
		})

		local on_attach = require("common").lsp_on_attach

		-- grammar
		vim.lsp.config("harper_ls", {
			settings = {
				["harper-ls"] = {
					linters = {
						SpellCheck = false,
						SentenceCapitalization = false,
					},
				},
			},
		})

		-- JavaScript/TypeScript
		vim.lsp.config("ts_ls", { on_attach = on_attach })

		-- Lua
		vim.lsp.config("lua_ls", {
			on_attach = on_attach,
			settings = {
				Lua = {
					runtime = {
						-- Tell the language server which version of Lua you're using
						-- (most likely LuaJIT in the case of Neovim)
						version = "LuaJIT",
					},
					hint = { enable = true },
					diagnostics = {
						-- Get the language server to recognize the `vim` global
						globals = { "vim" },
					},
					-- Make the server aware of Neovim runtime files
					workspace = { library = vim.api.nvim_get_runtime_file("", true) },
					-- Do not send telemetry data containing a randomized but unique identifier
					telemetry = { enable = false },
				},
			},
		})

		-- nix
		vim.lsp.config("nixd", {
			on_attach = on_attach,
			settings = {
				nixd = {
					nixpkgs = {
						expr = 'import (builtins.getFlake ("/Users/nixypanda/.dotfiles")).inputs.nixpkgs {}',
					},
					diagnostic = { suppress = { "sema-escaping-with" } },
					options = {
						nixos = {
							expr = '(builtins.getFlake ("/Users/nixypanda/.dotfiles")).nixosConfigurations.nixos.options',
						},
						home_manager = {
							expr = '(builtins.getFlake ("/Users/nixypanda/.dotfiles")).homeConfigurations."srt-l02-sekhmet".options',
						},
					},
				},
			},
		})

		-- nu
		vim.lsp.config("nushell", { on_attach = on_attach })

		-- Python
		vim.lsp.config("ruff", { on_attach = on_attach })
		vim.lsp.config("pyright", { on_attach = on_attach })

		-- Terraform
		vim.lsp.config("terraform_lsp", { on_attach = on_attach })

		-- Shit you need to deal with
		vim.lsp.config("bashls", { on_attach = on_attach })
		vim.lsp.config("cmake", { on_attach = on_attach })
		vim.lsp.config("cssls", { on_attach = on_attach })
		vim.lsp.config("dockerls", { on_attach = on_attach })
		vim.lsp.config("html", { on_attach = on_attach })

		-- json/yaml/TOML configs
		vim.lsp.config("jsonls", {
			on_attach = on_attach,
			settings = {
				json = {
					schemas = require("schemastore").json.schemas(),
					validate = { enable = true },
				},
			},
		})

		vim.lsp.config("yamlls", {
			on_attach = on_attach,
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
		vim.lsp.config("taplo", { on_attach = on_attach })

		vim.lsp.enable("harper_ls")
		vim.lsp.enable("ts_ls")
		vim.lsp.enable("lua_ls")
		vim.lsp.enable("nixd")
		vim.lsp.enable("nushell")
		vim.lsp.enable("ruff")
		vim.lsp.enable("pyright")
		vim.lsp.enable("cssls")
		vim.lsp.enable("terraform_lsp")
		vim.lsp.enable("bashls")
		vim.lsp.enable("cmake")
		vim.lsp.enable("dockerls")
		vim.lsp.enable("html")
		vim.lsp.enable("jsonls")
		vim.lsp.enable("yamlls")
		vim.lsp.enable("taplo")
	end,
})
