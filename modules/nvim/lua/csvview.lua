require("lz.n").load({
	"csvview.nvim",
	cmd = { "CsvViewEnable", "CsvViewDisable", "CsvViewToggle", "CsvViewInfo" },
	ft = { "csv", "tsv" },
	keys = {
		{ "<leader>uc", "<cmd>CsvViewToggle<cr>", desc = "Toggle CSV View" },
	},
	after = function()
		require("csvview").setup({
			keymaps = {
				textobject_field_inner = { "if", mode = { "o", "x" } },
				textobject_field_outer = { "af", mode = { "o", "x" } },
			},
		})
	end,
})
