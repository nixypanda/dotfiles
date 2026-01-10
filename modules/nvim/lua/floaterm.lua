require("lz.n").load({
	"floaterm",
	keys = {
		{ "<leader>ft", "<cmd>FloatermToggle<cr>", desc = "Toggle Floating Terminal" },
	},
	after = function() require("floaterm").setup({}) end,
})
