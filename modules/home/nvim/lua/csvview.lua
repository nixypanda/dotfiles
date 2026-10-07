require("lz.n").load({
	"csvview.nvim",
	cmd = { "CsvViewEnable", "CsvViewDisable", "CsvViewToggle", "CsvViewInfo" },
	ft = { "csv", "tsv" },
	after = function()
		require("csvview").setup({
			keymaps = {
				textobject_field_inner = { "if", mode = { "o", "x" } },
				textobject_field_outer = { "af", mode = { "o", "x" } },
			},
		})
		vim.keymap.set("n", "<leader>uc", "<cmd>CsvViewToggle<cr>", { buffer = true, desc = "Toggle CSV View" })
	end,
})
