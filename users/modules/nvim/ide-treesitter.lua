require 'nvim-treesitter.configs'.setup {
    -- Note: installing nix grammer requires treesitter installed as command line too
    ensure_installed = {
        "bash", "c", "css", "dockerfile", "elm", "go", "haskell", "hcl", "html", "java",
        "javascript", "json", "latex", "lua", "nix", "python", "regex", "ruby", "rust",
        "scss", "toml", "tsx", "typescript", "yaml"
    },
    highlight = { enable = true },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "gnn",
            node_incremental = "grn",
            scope_incremental = "grc",
            node_decremental = "grm"
        }
    },
    indent = { enable = true },
    refactor = { highlight_definitions = { enable = true } },
    textobjects = {
        select = {
            enable = true,
            keymaps = {
                ["af"] = "@function.outer",
                ["if"] = "@function.inner",
                ["ac"] = "@class.outer",
                ["ic"] = "@class.inner",
                ["ab"] = "@block.outer",
                ["ib"] = "@block.inner",
                ["aa"] = "@parameter.outer",
                ["ia"] = "@parameter.inner"
            }
        },
        swap = {
            enable = true,
            swap_next = { ["<leader>npn"] = "@parameter.inner" },
            swap_previous = { ["<leader>npp"] = "@parameter.inner" }
        }
    }
}
