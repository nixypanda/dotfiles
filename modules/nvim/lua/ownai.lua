require("ownai").setup({
	default_mode = "signatures",
})

vim.keymap.set("n", "<leader>ot", "<cmd>OwnaiShow types<cr>", { desc = "OwnAI: show types" })
vim.keymap.set("n", "<leader>os", "<cmd>OwnaiShow signatures<cr>", { desc = "OwnAI: show signatures" })
vim.keymap.set("n", "<leader>ou", "<cmd>OwnaiFold full<cr>", { desc = "OwnAI: show full source" })
vim.keymap.set("n", "<leader>oo", "<cmd>OwnaiOutline<cr>", { desc = "OwnAI: declaration outline" })
vim.keymap.set("n", "<leader>oz", "<cmd>OwnaiToggle<cr>", { desc = "OwnAI: toggle auto-fold" })
