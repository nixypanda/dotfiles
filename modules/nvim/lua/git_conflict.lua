require("git-conflict").setup({})

vim.keymap.set("n", "<leader>gcb", "<cmd>GitConflictChooseBoth<cr>", { desc = "Choose both" })
vim.keymap.set("n", "<leader>gch", "<cmd>GitConflictChooseOurs<cr>", { desc = "Choose ours" })
vim.keymap.set("n", "<leader>gcl", "<cmd>GitConflictChooseTheirs<cr>", { desc = "Choose theirs" })
vim.keymap.set("n", "<leader>gcn", "<cmd>GitConflictConflictNone<cr>", { desc = "Choose none" })
vim.keymap.set("n", "<leader>gcj", "<cmd>GitConflictNextConflict<cr>", { desc = "Next conflict" })
vim.keymap.set("n", "<leader>gck", "<cmd>GitConflictPrevConflict<cr>", { desc = "Previous conflict" })
