require("lz.n").load({
	"todo-comments.nvim",
	event = { "BufReadPost", "BufNewFile" },
	keys = {
		{ "<leader>sT", "<cmd>TodoTelescope<cr>", desc = "Todo comments" },
	},
	after = function() require("todo-comments").setup() end,
})
