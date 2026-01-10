-- require "base-sane"
vim.o.mouse = "a"
vim.o.cursorline = true
vim.g.mapleader = " "

-- More natural pane splitting
vim.o.splitbelow = true
vim.o.splitright = true

-- Indentation
-- filetype plugin indent on
-- Show existing tab with 4 spaces width
vim.o.tabstop = 4
-- When indenting with '>', use 4 spaces width
vim.o.shiftwidth = 4
-- On pressing tab, insert 4 spaces
vim.o.expandtab = true

-- Use terminal GUI colors
vim.o.termguicolors = true

-- -- INSERT -- is unnecessary anymore because the mode information is
--  displayed in the statusline.
vim.o.showmode = false

-- allow hidden buffers so we can have unsaved worked that’s not displayed on your screen
vim.o.hidden = true

-- Some language servers have issues with back-up files
vim.o.backup = false
vim.o.writebackup = false

-- " Better display for messages
vim.o.cmdheight = 1

-- You will have bad experience for diagnostic messages when it's default 4000.
vim.o.updatetime = 300

-- " don't give |ins-completion-menu| messages.
vim.o.shortmess = vim.o.shortmess .. "c"

-- " always show signcolumns
vim.o.signcolumn = "yes"

-- Setup whitespace chars
vim.opt.listchars = {
	eol = "¬",
	tab = ">·",
	trail = "~",
	extends = ">",
	precedes = "<",
	space = ".",
}

vim.o.shell = "nu"

-- More prominent splits. This is a general problem with a lot of the themes
vim.opt.fillchars = {
	vert = "█",
	horiz = "▀",
	horizup = "█",
	horizdown = "█",
	vertleft = "█",
	vertright = "█",
	verthoriz = "█",
}

-- Set relative numbering
vim.wo.number = true
vim.wo.relativenumber = true

-- require "look-lsp"
vim.diagnostic.config({
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = " ",
			[vim.diagnostic.severity.WARN] = " ",
			[vim.diagnostic.severity.INFO] = " ",
			[vim.diagnostic.severity.HINT] = " ",
		},
	},
	update_in_insert = false,
	severity_sort = false,
	float = { source = "if_many", border = "rounded" },
	jump = { float = true },
})

-- require "look-theme"
vim.filetype.add({
	extension = {
		rasi = "scss",
		tfstate = "json",
		nomad = "hcl",
	},
	filename = {
		["flake.lock"] = "json",
		["manifest"] = "hcl",
		["lotus58.keymap"] = "c",
		["lotus58.conf"] = "c",
	},
})

-- This is a very specific setting which sets the color of border to be the
-- dark background of the tokyonight theme. This is done because we want to
-- have a seemless split between the filetree plugin and the buffer. As a side
-- effect for this we also get prominent splits as we need to use huge
-- forground blocks to set this up.
vim.g.tokyonight_colors = {
	border = "#1f2335",
}

-- helpful buffer operations
vim.keymap.set("n", "<leader>bA", "<cmd>bufdo bd<cr>", { desc = "Close all buffers" })
vim.keymap.set("n", "<leader>bc", "<cmd>BufferClose<cr>", { desc = "Close this buffer" })
vim.keymap.set("n", "<leader>bC", "<cmd>w | %bd | e#<cr>", { desc = "Close all other buffers" })
vim.keymap.set("n", "<leader>bj", "<cmd>bnext<cr>", { desc = "Next buffer" })
vim.keymap.set("n", "<leader>bk", "<cmd>bperv<cr>", { desc = "Previous buffer" })
vim.keymap.set("n", "<leader>bs", "<cmd>tab split<cr>", { desc = "Split to tab" })
vim.keymap.set("n", "<leader>bS", "<cmd>tab close<cr>", { desc = "Close tab split" })

-- utilities
vim.keymap.set("n", "<leader>uh", '<cmd>let @/ = ""<cr>', { desc = "Remove search highlight" })

-- helpful window operations
vim.keymap.set("n", "<leader>wv", "<C-w>t<C-w>H", { desc = "Horizontally split windows to vertical" })
vim.keymap.set("n", "<leader>wv", "<C-w>t<C-w>K", { desc = "Vertically split windows to horizontal" })

-- quickfix
vim.keymap.set("n", "<leader>co", "<cmd>copen<cr>", { desc = "Open quickfix list" })
vim.keymap.set("n", "<leader>cj", "<cmd>cnext<cr>", { desc = "Next quickfix item" })
vim.keymap.set("n", "<leader>ck", "<cmd>cprev<cr>", { desc = "Previous quickfix item" })
vim.keymap.set("n", "<leader>cl", "<cmd>colder<cr>", { desc = "Older quickfix list" })
vim.keymap.set("n", "<leader>ch", "<cmd>cnewer<cr>", { desc = "Newer quickfix list" })
