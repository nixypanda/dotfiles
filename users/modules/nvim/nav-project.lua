vim.g.nvim_tree_respect_buf_cwd = 1

require("project_nvim").setup {
    -- All the patterns used to detect root dir, when **"pattern"** is in
    -- detection_methods
    patterns = {
        "flake.nix", " .git", "_darcs", ".hg", ".bzr", ".svn", "Makefile",
        "package.json"
    }
}

require("nvim-tree").setup({
    update_cwd = true,
    update_focused_file = { enable = true, update_cwd = true }
})

require('telescope').load_extension('projects')
