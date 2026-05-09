vim.keymap.set("n", "<leader>bA", "<cmd>bufdo bd<cr>", { desc = "Close all buffers" })
vim.keymap.set("n", "<leader>bc", "<cmd>BufferClose<cr>", { desc = "Close this buffer" })
vim.keymap.set("n", "<leader>bC", "<cmd>w | %bd | e#<cr>", { desc = "Close all other buffers" })
vim.keymap.set("n", "<leader>bj", "<cmd>bnext<cr>", { desc = "Next buffer" })
vim.keymap.set("n", "<leader>bk", "<cmd>bprev<cr>", { desc = "Previous buffer" })
vim.keymap.set("n", "<leader>bs", "<cmd>tab split<cr>", { desc = "Split to tab" })
vim.keymap.set("n", "<leader>bS", "<cmd>tab close<cr>", { desc = "Close tab split" })

vim.keymap.set("n", "<leader>uh", '<cmd>let @/ = ""<cr>', { desc = "Remove search highlight" })

vim.keymap.set("n", "<leader>wv", "<C-w>t<C-w>H", { desc = "Move split layout to vertical" })
vim.keymap.set("n", "<leader>wh", "<C-w>t<C-w>K", { desc = "Move split layout to horizontal" })

vim.keymap.set("n", "<leader>co", "<cmd>copen<cr>", { desc = "Open quickfix list" })
vim.keymap.set("n", "<leader>cj", "<cmd>cnext<cr>", { desc = "Next quickfix item" })
vim.keymap.set("n", "<leader>ck", "<cmd>cprev<cr>", { desc = "Previous quickfix item" })
vim.keymap.set("n", "<leader>cl", "<cmd>colder<cr>", { desc = "Older quickfix list" })
vim.keymap.set("n", "<leader>ch", "<cmd>cnewer<cr>", { desc = "Newer quickfix list" })
