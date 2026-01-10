require("lz.n").load({
	"treesj",
	keys = {
		{ "<leader>us", function() require("treesj").toggle() end, desc = "Toggle split-join" },
		{
			"<leader>uS",
			function() require("treesj").toggle({ split = { recursive = true } }) end,
			desc = "Toggle split-join recursive",
		},
	},
	after = function() require("treesj").setup() end,
})
