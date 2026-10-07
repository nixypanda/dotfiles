require("lz.n").load({
	"render-markdown.nvim",
	ft = { "markdown", "Avante" },
	after = function() require("render-markdown").setup({}) end,
})
