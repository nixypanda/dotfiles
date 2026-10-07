require("lz.n").load({
	"blame.nvim",
	keys = {
		{ "<leader>gB", "<cmd>BlameToggle window<cr>", desc = "Git blame" },
	},
	after = function() require("blame").setup() end,
})
