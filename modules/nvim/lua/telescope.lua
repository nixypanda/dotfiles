require("lz.n").load({
	"telescope.nvim",
	cmd = "Telescope",
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("telescope-ui-select.nvim")
		vim.cmd.packadd("telescope-fzf-native.nvim")
	end,
	keys = {
		-- Searching
		{ "<leader>sb", "<cmd>Telescope buffers<cr>", desc = "Open [B]uffers" },
		{ "<leader>sc", "<cmd>Telescope command_history<cr>", desc = "Previous [c]ommands" },
		{ "<leader>sC", "<cmd>Telescope commands<cr>", desc = "Available [C]ommands" },
		{ "<leader>sf", "<cmd>Telescope find_files<cr>", desc = "[F]iles" },
		{ "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "[H]elp Tags" },
		{ "<leader>sj", "<cmd>Telescope jumplist<cr>", desc = "[J]ump List" },
		{ "<leader>sm", "<cmd>Telescope marks<cr>", desc = "[M]arks" },
		{ "<leader>sr", "<cmd>Telescope resume<cr>", desc = "[R]esume" },
		{ "<leader>sR", "<cmd>Telescope registers<cr>", desc = "[R]egisters" },
		{ "<leader>st", "<cmd>Telescope live_grep<cr>", desc = "[T]ext" },
	},
	after = function()
		require("telescope").setup({
			extensions = {
				fzf = {
					fuzzy = true, -- false will only do exact matching
					override_generic_sorter = true, -- override the generic sorter
					override_file_sorter = true, -- override the file sorter
					case_mode = "smart_case", -- or "ignore_case" or "respect_case"
					-- the default case_mode is "smart_case"
				},
			},
		})
		require("telescope").load_extension("fzf")
		require("telescope").load_extension("ui-select")
	end,
})
