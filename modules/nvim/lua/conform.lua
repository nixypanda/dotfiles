require("lz.n").load({
	"conform.nvim",
	event = "BufWritePost",
	after = function()
		require("conform").setup({
			formatters_by_ft = {
				css = { "prettier" },
				go = { "gofmt" },
				html = { "prettier" },
				javascript = { "prettier" },
				json = { "prettier" },
				lua = { "stylua" },
				markdown = { "prettier", "markdownlint" },
				nix = { "nixfmt" },
				yaml = { "prettier" },
			},
			format_on_save = {
				-- I recommend these options. See :help conform.format for details.
				lsp_fallback = true,
				timeout_ms = 500,
			},
		})
	end,
})
