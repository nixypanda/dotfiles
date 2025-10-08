require("lz.n").load({
	"render-markdown.nvim",
	ft = { "markdown", "Avante" },
	after = function() require("render-markdown").setup({ file_types = { "markdown", "Avante" } }) end,
})
