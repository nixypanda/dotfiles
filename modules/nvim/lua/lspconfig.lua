require("lz.n").load({
	"nvim-lspconfig",
	event = { "BufReadPre", "BufNewFile" },
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("blink.cmp")
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

			local jump_to_next_error = function()
				require("lspsaga.diagnostic"):goto_next({ severity = vim.diagnostic.severity.ERROR })
			end
			local jump_to_prev_error = function()
				require("lspsaga.diagnostic"):goto_prev({ severity = vim.diagnostic.severity.ERROR })
			end

			-- mutations
			map("<leader>lC", vim.lsp.codelens.run, "Code Lens")
			map("<leader>lR", "<cmd>Lspsaga rename<cr>", "Rename")
			map("<leader>la", "<cmd>Lspsaga code_action<cr>", "Code Action")

			-- finding stuff
			map("<leader>lO", "<cmd>Lspsaga outline<cr>", "Toggle Document Symbols Outline")
			map("<leader>lS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", "Workspace Symbols")
			map("<leader>lf", "<cmd>Lspsaga finder<cr>", "Finder: Refrences and implementations")
			map("<leader>li", "<cmd>Telescope lsp_incoming_calls<cr>", "Incoming Calls")
			map("<leader>lo", "<cmd>Telescope lsp_outgoing_calls<cr>", "Outgoing Calls")
			map("<leader>lr", "<cmd>Telescope lsp_references<cr>", "Refrences")
			map("<leader>ls", "<cmd>Telescope document_symbols<cr>", "Document Symbols")
			map("<leader>lp", "<cmd>Lspsaga hover_doc<cr>", "Preview Definition")

			-- diagnostics
			map("<leader>lc", "<cmd>Lspsaga show_cursor_diagnostics<cr>", "Cursor Diagnostics")
			map("<leader>ld", "<cmd>Telescope diagnostics bufnr=0<cr>", "Document Diagnostics")
			map("<leader>lD", "<cmd>Telescope diagnostics<cr>", "Workspace Diagnostics")
			map("<leader>lL", "<cmd>Lspsaga show_line_diagnostics<cr>", "Line Diagnostics")
			-- diagnostics navigation
			map("<leader>lh", "<cmd>Lspsaga diagnostic_jump_prev<cr>", "Previous diagnostic")
			map("<leader>ll", "<cmd>Lspsaga diagnostic_jump_next<cr>", "Next diagnostic")
			map("<leader>lj", jump_to_next_error, "Next error diagnostic")
			map("<leader>lk", jump_to_prev_error, "Previous error diagnostic")

			-- auxiliary
			map("<leader>lI", "<cmd>LspInfo<cr>", "Info")
			map("<leader>lu", "<cmd>LspRestart<cr>", "Restart LSP")
			map("<leader>lU", "<cmd>LspStart<cr>", "Start LSP")

			-- Other mode keymaps
			vim.keymap.set(
				"v",
				"<leader>la",
				"<cmd>LspSaga code_action<cr>",
				{ buffer = bufnr, desc = "Code [A]ction" }
			)
		end

		-- Need to add this to the language server to broadcast snippet compatibility
		local capabilities = require("blink.cmp").get_lsp_capabilities()

		lspconfig.gleam.setup({
			capabilities = capabilities,
			on_attach = on_attach,
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

		-- JavaScript/TypeScript
		lspconfig.ts_ls.setup({ capabilities = capabilities, on_attach = on_attach })

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
		lspconfig.nushell.setup({ capabilities = capabilities, on_attach = on_attach })

		-- Python
		lspconfig.ruff.setup({ capabilities = capabilities, on_attach = on_attach })
		lspconfig.pyright.setup({ capabilities = capabilities, on_attach = on_attach })
		-- lspconfig.basedpyright.setup({ capabilities = capabilities, })
		--
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
		lspconfig.terraform_lsp.setup({ capabilities = capabilities, on_attach = on_attach })

		-- shit you need to deal with
		lspconfig.bashls.setup({ capabilities = capabilities, on_attach = on_attach })
		lspconfig.cmake.setup({ capabilities = capabilities, on_attach = on_attach })
		lspconfig.cssls.setup({ capabilities = capabilities, on_attach = on_attach })
		lspconfig.dockerls.setup({ capabilities = capabilities, on_attach = on_attach })
		lspconfig.html.setup({ capabilities = capabilities, on_attach = on_attach })

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
		lspconfig.taplo.setup({ capabilities = capabilities, on_attach = on_attach })

		-- Stuff outside of lspconfig
		vim.g.haskell_tools = {
			hls = {
				capabilities = capabilities,
				on_attach = function(client, bufnr)
					on_attach(client, bufnr)

					local map = function(keys, func, desc)
						vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
					end
					local ht = require("haskell-tools")

					map("<leader>ps", ht.hoogle.hoogle_signature, "Hoogle search type signature")
					map("<leader>pe", ht.lsp.buf_eval_all, "Evaluate all code snippets")
					map("<leader>pr", ht.repl.toggle, "Toggle GHCi repl (package)")
					map(
						"<leader>pR",
						function() ht.repl.toggle(vim.api.nvim_buf_get_name(0)) end,
						"Toggle GHCi repl (buffer)"
					)
				end,
				debug = true,
				cmd = {
					"haskell-language-server-wrapper",
					"--lsp",
					"--log-level",
					"Warning",
					"--logfile",
					vim.fn.stdpath("log") .. "/" .. "haskell-language-server.log",
				},
			},
		}

		vim.g.rustaceanvim = {
			tools = {
				hover_actions = { auto_focus = true },
				test_executor = "neotest",
			},
			server = {
				capabilities = capabilities,
				on_attach = function(client, bufnr)
					on_attach(client, bufnr)

					local map = function(keys, func, desc)
						vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
					end

					map("<leader>pd", "<cmd>RustLsp debug<cr>", "Debug target")
					map("<leader>pD", "<cmd>RustLsp debuggables<cr>", "Debug available targets")
					map("<leader>pr", "<cmd>RustLsp run<cr>", "Run target")
					map("<leader>pR", "<cmd>RustLsp runnables<cr>", "Run available targets")
					map("<leader>pt", "<cmd>RustLsp testables<cr>", "Test available targets")
					map("<leader>pT", "<cmd>RustLsp workspaceSymbol onlyTypes<cr>", "Search types")
					map("<leader>pm", "<cmd>RustLsp expandMacro<cr>", "Expand macro")
				end,
				default_settings = {
					["rust-analyzer"] = {
						checkOnSave = { command = "clippy" },
						-- RA will scan .direnv correspondingly entire nixpkgs repository
						files = { excludeDirs = { ".direnv" } },
					},
				},
			},
			dap = {
				-- Injected by nix: codelldb_path and liblldb_path
				adapter = require("rustaceanvim.config").get_codelldb_adapter(codelldb_path, liblldb_path),
			},
		}
	end,
})
