require("lz.n").load({
	"diffview.nvim",
	keys = {
		{ "<leader>go", "<cmd>DiffviewOpen<cr>", desc = "Open Diffview" },
		{ "<leader>gc", "<cmd>DiffviewClose<cr>", desc = "Close Diffview" },
		{ "<leader>gf", "<cmd>DiffviewFileHistory<cr>", desc = "Open File History" },
		{ "<leader>gp", "<cmd>DiffviewOpen main..HEAD<cr>", desc = "Open branch diff with main" },
		{ "<leader>gP", "<cmd>DiffviewOpen master..HEAD<cr>", desc = "Open branch diff with master" },
	},
	after = function() require("diffview").setup({}) end,
})
