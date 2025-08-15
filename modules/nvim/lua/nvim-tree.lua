-- disable netrw at the very start of your init.lua
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

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

		-- Workaround to make the global statusline look shifted over when nvim tree is
		-- active
		local nvim_tree_shift = {
			function()
				local nvim_tree_window_number = require("nvim-tree.api").tree.winid() or 0
				return string.rep(" ", vim.api.nvim_win_get_width(nvim_tree_window_number) - 2)
			end,
			cond = require("nvim-tree.api").tree.is_visible,
			color = "NvimTreeNormal",
		}

		require("lualine").setup({
			sections = {
				lualine_a = { nvim_tree_shift, "mode" },
			},
		})
	end,
})
