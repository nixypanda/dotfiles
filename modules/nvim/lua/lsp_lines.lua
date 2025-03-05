require("lz.n").load({
	"lsp_lines.nvim",
	keys = {
		{ "<leader>lm", function() require("lsp_lines").toggle() end, desc = "Multi-line diagnostics view" },
	},
	after = function() require("lsp_lines").setup({}) end,
})
