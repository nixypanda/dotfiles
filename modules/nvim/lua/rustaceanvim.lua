local capabilities = require("cmp_nvim_lsp").default_capabilities()

vim.g.rustaceanvim = {
	tools = {
		hover_actions = { auto_focus = true },
	},
	server = {
		capabilities = capabilities,
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
