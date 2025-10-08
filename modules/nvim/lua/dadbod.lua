vim.g.db_ui_use_nerd_fonts = 1
vim.g.db_ui_win_position = "right"
vim.keymap.set("n", "<leader>Da", "<cmd>DBUIAddConnection<cr>", { desc = "Add new connection" })
vim.keymap.set("n", "<leader>Do", "<cmd>:tab DBUI<cr>", { desc = "Open DBUI" })
