local ht = require('haskell-tools')
local def_opts = { noremap = true, silent = true, }
ht.setup {
    hls = {
        default_settings = {
            haskell = { formattingProvider = "brittany" }
        }
    },
}

-- Suggested keymaps that do not depend on haskell-language-server:
local bufnr = vim.api.nvim_get_current_buf()

-- Detect nvim-dap launch configurations
-- (requires nvim-dap and haskell-debug-adapter)
-- ht.dap.discover_configurations(bufnr)
