local capabilities = require("cmp_nvim_lsp").default_capabilities()
local rt = require("rust-tools")

rt.setup({
	tools = {
		hover_actions = { auto_focus = true },
	},
	server = {
		capabilities = capabilities,
		settings = {
			["rust-analyzer"] = {
				checkOnSave = { command = "clippy" },
				-- RA will scan .direnv correspondingly entire nixpkgs repository
				files = { excludeDirs = { ".direnv" } },
			},
		},
	},
	dap = {
		-- Injected by nix: codelldb_path and liblldb_path
		adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path),
	},
})
