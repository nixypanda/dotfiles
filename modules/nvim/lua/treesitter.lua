require("nvim-treesitter.configs").setup({
	ensure_installed = {
		-- This needs to be empty otherwise treesitter complains about
		-- directory being not being writable. All the installation of the
		-- parsers is done declaratively into an immutable location using nix,
		-- so we don't really need to specify anything there.
		-- https://github.com/NixOS/nixpkgs/issues/189838
	},
	highlight = { enable = true },
	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = "gnn",
			node_incremental = "grn",
			scope_incremental = "grc",
			node_decremental = "grm",
		},
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
				["ia"] = "@parameter.inner",
			},
		},
		swap = {
			enable = true,
			swap_next = { ["<leader>npn"] = "@parameter.inner" },
			swap_previous = { ["<leader>npp"] = "@parameter.inner" },
		},
	},
})

-- Set filetype to "nu" for files named "*.nu"
vim.filetype.add({ extension = { nu = "nu" } })
-- Register the nu parser for files with "nu" filetype
vim.treesitter.language.register("nu", "nu")
-- Add our nu parser to treesitter and associate it with nu filetype.
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.nu = {
	filetype = "nu",
}
