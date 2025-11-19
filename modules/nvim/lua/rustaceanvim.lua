vim.g.rustaceanvim = {
	tools = {
		hover_actions = { auto_focus = true },
	},
	server = {
		on_attach = function(client, bufnr)
			require("common").lsp_on_attach(client, bufnr)

			local map = function(keys, func, desc) vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc }) end

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
				-- Rust Analyzer will scan .direnv correspondingly entire nixpkgs repository
				files = { excludeDirs = { ".direnv" } },
			},
		},
	},
	dap = {
		-- Injected by nix: codelldb_path and liblldb_path
		adapter = require("rustaceanvim.config").get_codelldb_adapter(
			require("nix_injected").rustaceanvim_codelldb_path,
			require("nix_injected").rustaceanvim_liblldb_path
		),
	},
}
