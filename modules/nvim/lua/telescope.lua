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
		{ "<leader>sb", "<cmd>Telescope buffers<cr>", desc = "Open Buffers" },
		{ "<leader>sc", "<cmd>Telescope command_history<cr>", desc = "Previous commands" },
		{ "<leader>sC", "<cmd>Telescope commands<cr>", desc = "Available Commands" },
		{ "<leader>sf", "<cmd>Telescope find_files<cr>", desc = "Files" },
		{ "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "Help Tags" },
		{ "<leader>sj", "<cmd>Telescope jumplist<cr>", desc = "Jump List" },
		{ "<leader>sm", "<cmd>Telescope marks<cr>", desc = "Marks" },
		{ "<leader>sr", "<cmd>Telescope resume<cr>", desc = "Resume" },
		{ "<leader>sR", "<cmd>Telescope registers<cr>", desc = "Registers" },
		{ "<leader>st", "<cmd>Telescope live_grep<cr>", desc = "Text" },
	},
	after = function()
		require("telescope").setup({
			extensions = {
				fzf = {
					fuzzy = true,
					override_generic_sorter = true,
					override_file_sorter = true,
					case_mode = "smart_case", -- or "ignore_case" or "respect_case"
				},
			},
		})
		require("telescope").load_extension("fzf")
		require("telescope").load_extension("ui-select")
	end,
})
