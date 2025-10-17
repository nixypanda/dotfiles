vim.g.table_mode_disable_mappings = 1
vim.g.table_mode_disable_tableize_mappings = 1
require("lz.n").load({
	"vim-table-mode",
	keys = { { "<leader>ut", "<cmd>TableModeToggle<cr>", desc = "Toggle Table Mode" } },
})
