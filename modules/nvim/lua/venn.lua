-- venn.nvim: enable or disable keymappings

local keymappings_option = { noremap = true, silent = true }

function _G.Toggle_venn()
    local venn_enabled = vim.inspect(vim.b.venn_enabled)
    if venn_enabled == "nil" then
        vim.b.venn_enabled = true
        vim.cmd [[setlocal ve=all]]
        -- draw a line on HJKL keystokes
        vim.api.nvim_buf_set_keymap(0, "n", "J", "<C-v>j:VBox<CR>", keymappings_option)
        vim.api.nvim_buf_set_keymap(0, "n", "K", "<C-v>k:VBox<CR>", keymappings_option)
        vim.api.nvim_buf_set_keymap(0, "n", "L", "<C-v>l:VBox<CR>", keymappings_option)
        vim.api.nvim_buf_set_keymap(0, "n", "H", "<C-v>h:VBox<CR>", keymappings_option)
        -- draw a box by pressing "f" with visual selection
        vim.api.nvim_buf_set_keymap(0, "v", "f", ":VBox<CR>", keymappings_option)
    else
        vim.cmd [[setlocal ve=]]
        vim.cmd [[mapclear <buffer>]]
        vim.b.venn_enabled = nil
    end
end

-- toggle keymappings for venn using <leader>v

vim.api.nvim_set_keymap('n', '<leader>v', ":lua Toggle_venn()<CR>", { noremap = true })
