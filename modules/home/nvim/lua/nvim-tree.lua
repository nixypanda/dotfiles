require("lz.n").load({
	"nvim-tree.lua",
	keys = {
		{ "<leader>ec", "<cmd>NvimTreeCollapse<cr>", desc = "Collapse" },
		{ "<leader>ee", "<cmd>NvimTreeToggle<cr>", desc = "Toggle" },
		{ "<leader>er", "<cmd>NvimTreeRefresh<cr>", desc = "Refresh" },
	},
	after = function()
		require("nvim-tree").setup({
			-- update the focused file on `BufEnter`, un-collapses the folders
			-- recursively until it finds the file
			update_focused_file = { enable = true },
			renderer = {
				indent_markers = { enable = true },
			},
		})
	end,
})
