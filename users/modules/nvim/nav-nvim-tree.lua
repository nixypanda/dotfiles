require'nvim-tree'.setup {
    -- update the focused file on `BufEnter`, un-collapses the folders
    -- recursively until it finds the file
    update_focused_file = {enable = true}
}
vim.g.nvim_tree_indent_markers = 1

vim.g.nvim_tree_width = 30

Nvim_tree_toggle = function()
    require'nvim-tree'.toggle()

    if require'nvim-tree.view'.win_open() then
        require'bufferline.state'.set_offset(vim.g.nvim_tree_width + 1, 'File Tree')
        require('nvim-tree').find_file(true)
    else
        require'bufferline.state'.set_offset(0)
    end
end

