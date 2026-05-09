local injected = require("nix_injected")

local config = {
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
}

if injected.rustaceanvim_codelldb_path and injected.rustaceanvim_liblldb_path then
	config.dap = {
		adapter = require("rustaceanvim.config").get_codelldb_adapter(
			injected.rustaceanvim_codelldb_path,
			injected.rustaceanvim_liblldb_path
		),
	}
end

vim.g.rustaceanvim = config
