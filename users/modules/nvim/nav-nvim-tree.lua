require 'nvim-tree'.setup {
    -- update the focused file on `BufEnter`, un-collapses the folders
    -- recursively until it finds the file
    update_focused_file = { enable = true }
}
vim.g.nvim_tree_indent_markers = 1

vim.g.nvim_tree_width = 30
