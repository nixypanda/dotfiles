vim.g.mapleader = " "

vim.o.mouse = "a"
vim.o.cursorline = true
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = true
vim.o.termguicolors = true
vim.o.showmode = false
vim.o.hidden = true
vim.o.backup = false
vim.o.writebackup = false
vim.o.cmdheight = 1
vim.o.updatetime = 300
vim.o.shortmess = vim.o.shortmess .. "c"
vim.o.signcolumn = "yes"
vim.o.shell = "nu"

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.opt.listchars = {
	eol = "¬",
	tab = ">·",
	trail = "~",
	extends = ">",
	precedes = "<",
	space = ".",
}

vim.opt.fillchars = {
	vert = "█",
	horiz = "▀",
	horizup = "█",
	horizdown = "█",
	vertleft = "█",
	vertright = "█",
	verthoriz = "█",
}

vim.wo.number = true
vim.wo.relativenumber = true

vim.g.tokyonight_colors = {
	border = "#1f2335",
}
