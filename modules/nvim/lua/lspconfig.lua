require("lz.n").load({
	"nvim-lspconfig",
	event = { "BufReadPre", "BufNewFile" },
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("cmp-nvim-lsp")
		vim.cmd.packadd("lspsaga.nvim")
		vim.cmd.packadd("SchemaStore.nvim")
	end,
	after = function()
		local lspconfig = require("lspconfig")

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
		})

		local on_attach = function(client, bufnr)
			-- inlay hints
			if client.server_capabilities.inlayHintProvider then vim.lsp.inlay_hint.enable() end

			-- Set keymaps
			local map = function(keys, func, desc) vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc }) end

			map("gd", "<cmd>lua vim.lsp.buf.definition()<cr>", "Goto Definition")
			map("gi", "<cmd>lua vim.lsp.buf.implementation()<cr>", "Goto Implementation")
			-- Lspconfig utils
			map("<leader>lI", "<cmd>LspInfo<cr>", "Info")
			map("<leader>lu", "<cmd>LspRestart<cr>", "Restart LSP")
			map("<leader>lU", "<cmd>LspStart<cr>", "Start LSP")
			-- Telescope
			map("<leader>ld", "<cmd>Telescope diagnostics bufnr=0<cr>", "Document Diagnostics")
			map("<leader>lD", "<cmd>Telescope diagnostics<cr>", "Workspace Diagnostics")
			map("<leader>ls", "<cmd>Telescope document_symbols<cr>", "Document Symbols")
			map("<leader>lS", "<cmd>Telescope sp_dynamic_workspace_symbols<cr>", "Workspace Symbols")

			-- Lspsaga
			map("<leader>la", "<cmd>Lspsaga code_action<cr>", "Code Action")
			map("<leader>lc", "<cmd>Lspsaga show_cursor_diagnostics<cr>", "Cursor Diagnostics")
			map("<leader>lf", "<cmd>Lspsaga finder<cr>", "Finder: Refrences and implementations")
			map("<leader>li", "<cmd>Lspsaga incoming_calls<cr>", "Incoming Calls")
			map("<leader>lj", "<cmd>Lspsaga diagnostic_jump_next<cr>", "Next Action")
			map("<leader>lk", "<cmd>Lspsaga diagnostic_jump_prev<cr>", "Previous Action")
			map("<leader>ll", "<cmd>Lspsaga show_line_diagnostics<cr>", "Line Diagnostics")
			map("<leader>lo", "<cmd>Lspsaga outgoing_calls<cr>", "Outgoing Calls")
			map("<leader>lO", "<cmd>Lspsaga outline<cr>", "Toggle Document Symbols Outline")
			map("<leader>lp", "<cmd>Lspsaga hover_doc<cr>", "Preview Definition")
			map("<leader>lr", "<cmd>Lspsaga rename<cr>", "Rename")

			-- Other mode keymaps
			vim.keymap.set(
				"v",
				"<leader>la",
				"<cmd>LspSaga code_action<cr>",
				{ buffer = bufnr, desc = "Code [A]ction" }
			)
		end

		-- Need to add this to the language server to broadcast snippet compatibility
		local capabilities = require("cmp_nvim_lsp").default_capabilities()

		-- go
		lspconfig.gopls.setup({
			capabilities = capabilities,
			on_attach = on_attach,
			settings = {
				gopls = {
					completeUnimported = true,
					usePlaceholders = true,
					analyses = {
						unusedparams = true,
					},
				},
				hints = {
					assignVariableTypes = true,
					compositeLiteralFields = true,
					compositeLiteralTypes = true,
					constantValues = true,
					functionTypeParameters = true,
					parameterNames = true,
					rangeVariableTypes = true,
				},
			},
		})

		-- grammer
		lspconfig.ltex.setup({
			capabilities = capabilities,
			on_attach = on_attach,
			settings = {
				ltex = {
					["ltex-ls"] = {
						logLevel = "warning",
					},
				},
			},
		})

		-- Java
		lspconfig.java_language_server.setup({
			cmd = { "java-language-server" },
			capabilities = capabilities,
			on_attach = on_attach,
		})

		-- JavaScript/TypeScript
		lspconfig.tsserver.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})

		-- lua
		lspconfig.lua_ls.setup({
			capabilities = capabilities,
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
		lspconfig.nixd.setup({
			capabilities = capabilities,
			on_attach = on_attach,
			settings = {
				nixd = {
					diagnostic = { suppress = { "sema-escaping-with" } },
					options = {
						nixos = {
							expr = '(builtins.getFlake ("~/.dotfiles")).nixosConfigurations.nixos.options',
						},
						home_manager = {
							expr = '(builtins.getFlake ("~/.dotfiles")).homeConfigurations."macbook-pro".options',
						},
					},
				},
			},
		})

		-- nu
		lspconfig.nushell.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})

		-- Python
		lspconfig.ruff.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
		lspconfig.pyright.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
		-- lspconfig.basedpyright.setup({
		-- 	capabilities = capabilities,
		-- })
		-- We are primarily using pyright for everything. Only using pylsp for rope refactors.
		-- lspconfig.pylsp.setup({
		-- 	settings = {
		-- 		pylsp = {
		-- 			plugins = {
		-- 				autopep8 = { enabled = false },
		-- 				jedi_completion = { enabled = false },
		-- 				jedi_definition = { enabled = false },
		-- 				jedi_hover = { enabled = false },
		-- 				jedi_references = { enabled = false },
		-- 				jedi_signature_help = { enabled = false },
		-- 				jedi_symbols = { enabled = false },
		-- 				maccabe = { enabled = false },
		-- 				preload = { enabled = false },
		-- 				pycodestyle = { enabled = false },
		-- 				pydocstyle = { enabled = false },
		-- 				pyflakes = { enabled = false },
		-- 				pylint = { enabled = false },
		-- 				rope_completion = { enabled = false },
		-- 				yapf = { enabled = false },
		-- 				-- using a fork of ropify for renameing modules etc,
		-- 				-- pyright already offers renaming capabilities that pylsp-rope offers at the moment
		-- 				pylsp_rope = { enabled = true, rename = false },
		-- 			},
		-- 		},
		-- 	},
		-- })

		-- Terraform
		lspconfig.terraform_lsp.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})

		-- shit you need to deal with
		lspconfig.bashls.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
		lspconfig.cmake.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
		lspconfig.cssls.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
		lspconfig.dockerls.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
		lspconfig.html.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})

		-- json/yaml/toml configs
		lspconfig.jsonls.setup({
			capabilities = capabilities,
			on_attach = on_attach,
			settings = {
				json = {
					schemas = require("schemastore").json.schemas(),
					validate = { enable = true },
				},
			},
		})

		lspconfig.yamlls.setup({
			capabilities = capabilities,
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
		lspconfig.taplo.setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
	end,
})
