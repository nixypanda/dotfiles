-- require "base-sane"
vim.o.mouse = "a"
-- vim.o.encoding = "UTF-8" -- redundant
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

-- " COC recommended defaults
-- " if hidden is not set, TextEdit might fail.
vim.o.hidden = true

-- Some language servers have issues with backup files
vim.o.backup = false
vim.o.writebackup = false

-- " Better display for messages
vim.o.cmdheight = 1

-- You will have bad experience for diagnostic messages when it's default 4000.
vim.o.updatetime = 300

-- " don't give |ins-completion-menu| messages.
vim.o.shortmess = vim.o.shortmess .. 'c'

-- " always show signcolumns
vim.o.signcolumn = 'yes'

-- " Vertical split in conflict resolution / NOTE: Causes config to break
-- vim.g.diffopt = vim.g.diffopt .. 'vertical'

-- Setup whitespace chars
vim.opt.listchars = {
    eol = '¬',
    tab = '>·',
    trail = '~',
    extends = '>',
    precedes = '<',
    space = '.'
}

-- " Add line length end indicator
-- vim.o.colorcolumn = '88'

vim.o.shell = 'zsh'

-- More prominent splits. This is a general problem with a lot of the themes
vim.opt.fillchars = {
    vert = '█',
    horiz = '▀',
    horizup = '█',
    horizdown = '█',
    vertleft = '█',
    vertright = '█',
    verthoriz = '█',
}

-- Set relative numbering
vim.wo.number = true
vim.wo.relativenumber = true

-- require "look-lsp"
vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    underline = true,
    update_in_insert = false,
    severity_sort = false
})

local diagnostic_symbol_map = {
    { name = "DiagnosticSignError", symbol = "☠ " },
    { name = "DiagnosticSignWarn", symbol = " " },
    { name = "DiagnosticSignInfo", symbol = "" },
    { name = "DiagnosticSignHint", symbol = "☛ " },
}

for _, elm in ipairs(diagnostic_symbol_map) do
    vim.fn.sign_define(
        elm.name,
        { texthl = elm.name, text = elm.symbol, numhl = elm.name }
    )
end

-- require "look-theme"
local file_syntax_map = {
    { pattern = "*.rasi",         syntax = "scss" },
    { pattern = "flake.lock",     syntax = "json" },
    { pattern = "*.tfstate",      syntax = "json" },
    { pattern = "*.nomad",        syntax = "hcl" },
    { pattern = "manifest",       syntax = "hcl" },
    { pattern = "lotus58.keymap", syntax = "c" },
    { pattern = "lotus58.conf",   syntax = "c" },
}

for _, elm in ipairs(file_syntax_map) do
    vim.api.nvim_create_autocmd(
        { "BufNewFile", "BufRead" },
        { pattern = elm.pattern, command = "set syntax=" .. elm.syntax }
    )
end

-- This is a very specific setting which sets the color of border to be the
-- dark background of the tokyonight theme. This is done because we want to
-- have a seemless split between the filetree plugin and the buffer. As a side
-- effect for this we also get prominent splits as we need to use huge
-- forground blocks to set this up.
vim.g.tokyonight_colors = {
    border = '#1f2335'
}
