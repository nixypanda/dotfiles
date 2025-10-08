require("nvim-treesitter.configs").setup({
	ensure_installed = {
		-- This needs to be empty otherwise treesitter complains about
		-- directory being not being writable. All the installation of the
		-- parsers is done declaratively into an immutable location using nix,
		-- so we don't really need to specify anything there.
		-- https://github.com/NixOS/nixpkgs/issues/189838
	},
	sync_install = false,
	auto_install = false,
	ignore_install = {},

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
			swap_next = {
				["<leader>mp"] = "@parameter.inner",
			},
			swap_previous = {
				["<leader>mP"] = "@parameter.inner",
			},
		},
	},
})
