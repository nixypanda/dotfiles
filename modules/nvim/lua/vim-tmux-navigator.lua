local opts = { silent = true }

vim.keymap.set({ "n", "i", "v", "t" }, "<C-h>", "<cmd>TmuxNavigateLeft<CR>", opts)
vim.keymap.set({ "n", "i", "v", "t" }, "<C-j>", "<cmd>TmuxNavigateDown<CR>", opts)
vim.keymap.set({ "n", "i", "v", "t" }, "<C-k>", "<cmd>TmuxNavigateUp<CR>", opts)
vim.keymap.set({ "n", "i", "v", "t" }, "<C-l>", "<cmd>TmuxNavigateRight<CR>", opts)
vim.keymap.set({ "n", "i", "v", "t" }, "<C-\\>", "<cmd>TmuxNavigatePrevious<CR>", opts)
