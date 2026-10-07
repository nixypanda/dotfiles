vim.g.db_ui_use_nerd_fonts = 1
vim.g.db_ui_win_position = "right"

require("lz.n").load({
	"vim-dadbod-ui",
	keys = {
		{ "<leader>Da", "<cmd>DBUIAddConnection<cr>", desc = "Add new connection" },
		{ "<leader>Do", "<cmd>tab DBUI<cr>", desc = "Open DBUI" },
	},
	load = function(name)
		vim.cmd.packadd("vim-dotenv")
		vim.cmd("silent! Dotenv")
		vim.cmd.packadd("vim-dadbod")
		vim.cmd.packadd(name)
		vim.cmd.packadd("vim-dadbod-completion")
		vim.cmd.packadd("nvim-dadbod-ssh")
	end,
	after = function()
		vim.api.nvim_create_autocmd("User", {
			pattern = "DBUIOpened",
			callback = function()
				vim.cmd("silent! Dotenv")
				vim.cmd("silent! normal R")
			end,
		})
	end,
})
