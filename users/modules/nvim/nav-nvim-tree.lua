require 'nvim-tree'.setup {
    -- update the focused file on `BufEnter`, un-collapses the folders
    -- recursively until it finds the file
    update_focused_file = { enable = true }
}
vim.g.nvim_tree_indent_markers = 1

vim.g.nvim_tree_width = 30

local nvim_tree_events = require('nvim-tree.events')
local bufferline_state = require('bufferline.state')

nvim_tree_events.on_tree_open(function()
    bufferline_state.set_offset(vim.g.nvim_tree_width)
end)

nvim_tree_events.on_tree_close(function()
    bufferline_state.set_offset(0)
end)
