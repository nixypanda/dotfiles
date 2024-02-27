local slow_format_filetypes = {}

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
		python = { "ruff_fix", "ruff_format" },
		terraform = { "terraform_fmt" },
		yaml = { "prettier" },
	},
	format_on_save = {
		-- I recommend these options. See :help conform.format for details.
		lsp_fallback = true,
		timeout_ms = 500,
	},
})

require("conform").formatters.ruff_fix = {
	prepend_args = { "--select", "I" },
}
