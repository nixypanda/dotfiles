require("lz.n").load({
	"conform.nvim",
	event = "BufWritePre",
	after = function()
		require("conform").setup({
			formatters_by_ft = {
				css = { "prettier" },
				elm = { "elm_format" },
				html = { "prettier" },
				javascript = { "prettier" },
				json = { "prettier" },
				lua = { "stylua" },
				markdown = { "prettier" },
				nix = { "nixfmt" },
				yaml = { "prettier" },
			},
			format_on_save = {
				lsp_fallback = true,
				timeout_ms = 500,
			},
		})
	end,
})
