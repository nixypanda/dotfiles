require("lz.n").load({
	"conform.nvim",
	event = "BufWritePre",
	after = function()
		require("conform").setup({
			formatters_by_ft = {
				css = { "biome" },
				elm = { "elm_format" },
				html = { "biome" },
				javascript = { "biome" },
				javascriptreact = { "biome" },
				json = { "biome" },
				jsonc = { "biome" },
				lua = { "stylua" },
				markdown = { "rumdl" },
				nix = { "nixfmt" },
				typescript = { "biome" },
				typescriptreact = { "biome" },
				yaml = { "yamlfmt" },
			},
			-- The built-in yamlfmt formatter in conform.nvim doesn't set stdin,
			-- so it would format a temp file while passing `-`. Force stdin.
			formatters = {
				yamlfmt = { stdin = true },
			},
			format_on_save = {
				lsp_fallback = true,
				timeout_ms = 500,
			},
		})
	end,
})
