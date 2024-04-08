-- disable netrw at the very start of your init.lua
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require("nvim-tree").setup({
	-- update the focused file on `BufEnter`, un-collapses the folders
	-- recursively until it finds the file
	update_focused_file = { enable = true },
	renderer = {
		indent_markers = { enable = true },
	},
})
